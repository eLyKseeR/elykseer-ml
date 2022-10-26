
(* open Elykseer *)
open Elykseer__Lxr
(* open Elykseer__Lxr.BackupPlanner *)
(* open Elykseer__Lxr.Assembly *)
(* open Elykseer__Lxr.Buffer *)
open Elykseer__Lxr.Configuration
open Elykseer__Lxr.Environment

open Elykseer_utils

let def_myid = 1234567890

let arg_verbose = ref false
let arg_files = ref []
let arg_metapath = ref "meta"
let arg_chunkpath = ref "lxr"
let arg_nchunks = ref 16
let arg_nproc = ref 1
let arg_myid = ref def_myid (* "1234567890" *)

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-x", Arg.Set_string arg_chunkpath, "sets output path for encrypted chunks");
    ("-o", Arg.Set_string arg_metapath, "sets output path for meta data");
    ("-n", Arg.Set_int arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-j", Arg.Set_int arg_nproc, "sets number of parallel processes");
    ("-i", Arg.Set_int arg_myid, "sets own identifier (positive number)");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

(** the minimum available size in an assembly *)
let aminsz = 127

(* let finalise_assembly e0 =
  let (a,b) = Assembly.finish (e0.cur_assembly) (e0.cur_buffer) in
  (* create key *)
  let k = Elykseer_crypto.Key256.mk () in
  let e1 = Environment.env_add_aid_key e0 a.aid (Elykseer_crypto.Key256.to_bytes k) in
  (* encrypt *)
  match Assembly.encrypt a b (e1.keys) with
  | None -> Printf.printf "failure on encrypting assembly %s\n" a.aid;
            e0
  | Some (a',b') ->
  (* extract to chunks *)
      Assembly.extract e1.config a' b' |> ignore;
      e1 *)

(* let finalise_and_recreate_assembly e0 =
  let e1 = finalise_assembly e0 in
  Environment.recreate_assembly e1 *)

let curr_assembly_sz n = Conversion.n2i (Assembly.assemblysize n)

(* let check_env e0 =
  let a = e0.cur_assembly in
  if (Conversion.n2i a.apos) + aminsz > curr_assembly_sz a.nchunks
    then (Printf.printf "finalising assembly %s\n" a.aid;
         finalise_and_recreate_assembly e0)
    else e0 *)

let max_block_size = (128*1024)

(** how many blocks will the backup use?
    given: the file's size, max block size,
           number of chunks (i.e. size of an assembly),
           bytes free in the current assembly,
           the number of the assembly
    the output is:
           the last assembly number, how many bytes free in it,
           a list of triples: assembly number x file position x block size
*)
let rec prepare_blocks (anum,afree,acc) nchunks maxsz fpos fsz =
  Printf.printf "fsz: %d  afree: %d  maxsz: %d\n" fsz afree maxsz;
  if fsz <= afree
  then
    if fsz >= maxsz
      then prepare_blocks (anum,(afree - maxsz),(anum, fpos, maxsz) :: acc) nchunks maxsz (fpos + maxsz) (fsz - maxsz)
      else (* termination *)
        if fsz > 0 then (anum,afree - fsz, (anum, fpos, fsz) :: acc) else (anum,afree,acc)
  else (* fsz > afree *)
    if afree >= maxsz
    then prepare_blocks (anum,(afree - maxsz),(anum, fpos, maxsz) :: acc) nchunks maxsz (fpos + maxsz) (fsz - maxsz)
    else (* afree < maxsz *)
      let asz = curr_assembly_sz nchunks in
      if afree < aminsz
        (* continue in fresh assembly *)
      then prepare_blocks ((anum + 1),asz,acc) nchunks maxsz fpos fsz
      else prepare_blocks ((anum + 1),asz,(anum, fpos, afree) :: acc) nchunks maxsz (fpos + afree) (fsz - afree)

let analyse_file nchunks afree anum fn =
  (* Printf.printf "backup %s\n" fn; *)
  let fi = Filesupport.get_file_information fn in
  let (anum',afree',bs) = prepare_blocks (anum,afree,[]) nchunks max_block_size 0 (Conversion.n2i fi.fsize) in
  (anum',afree',List.rev bs)

let rec analyse_files' acc nchunks afree anum fns =
        match fns with
          [] -> acc
        | fn :: fns' -> let (anum',afree',bs) = analyse_file nchunks afree anum fn in
                        analyse_files' (bs :: acc) nchunks afree' anum' fns'
let analyse_files e0 fns =
  let a = e0.cur_assembly in
  let afree = curr_assembly_sz a.nchunks - (Conversion.n2i a.apos) in
  let bs = analyse_files' [] a.nchunks afree 1 fns in
  let bs' = List.rev bs in
  let fbs = Zip.zip fns bs' in
  List.iteri (fun i (fname,fblocks) ->
      Printf.printf "file %d: %s\n" i fname;
      List.iteri (fun j (n,p,x) -> Printf.printf "   %d: %d -> %d@%d\n" j n x p) fblocks
    ) fbs |> ignore;
  fbs

let count_files fbs =
  List.map fst fbs |> List.sort_uniq (compare) |> List.length
let count_assemblies fbs =
  List.map snd fbs |> List.flatten |> List.map (fun (a,_,_) -> a) |> List.sort_uniq (compare) |> List.length
(*
[
  ("tst1.dat", [ (1, 131072); (1, 131072) ]);
  ("tst2.dat", [ (1, 131072); (2, 131072) ])
]   
*)

let rec execute_backup e p anum fileblocks =
  match fileblocks with
  | [] -> e
  | (fname,blocks) :: r ->
    Printf.printf "    file: %s\n" fname;
    List.iteri (fun i (n,fpos,bsz) -> Printf.printf "      %d: anum: %d block: %d @ %d\n" i n bsz fpos) blocks;
    execute_backup e p anum r

let extract_fileblocks anum fileblocks =
  List.map (fun (fname,bs) -> (fname, List.filter (fun (n,_p,_x) -> n = anum) bs)) fileblocks

let rec run_backup e ps anum fileblocks =
  if anum < 1 then ()
  else
    match ps with
    | [] -> ()
    | p :: ps' -> 
      Printf.printf "executing backup of assembly %d\n" anum;
      let fileblocks' = extract_fileblocks anum fileblocks in
      let e' = execute_backup e p anum fileblocks' in
      run_backup e' (List.append ps' [p]) (anum - 1) fileblocks

let rec start_processes acc n =
  if n = 0 then List.rev acc else start_processes (n :: acc) (n - 1)

let run_distributed_backup e0 nproc acount fileblocks =
  (* start n processes *)
  let ps = start_processes [] nproc in
  (* distribute work among processes *)
  run_backup e0 ps acount fileblocks

(* main *)
let () = Arg.parse argspec anon_args_fun "lxr_backup: vxonji";
         let nchunks = Nchunks.from_int !arg_nchunks in
         if List.length !arg_files > 0
          && !arg_nproc > 0 && !arg_nproc < 65
         then
           (* let _setup = setup_environment in *)
           let myid = let id0 = !arg_myid in
                      if id0 >= 0 then id0 else def_myid in
           let conf : configuration = {
                         config_nchunks = nchunks;
                         path_chunks = !arg_chunkpath;
                         path_meta   = !arg_metapath;
                         my_id       = Conversion.i2n myid } in
           let e0 = Environment.initial_environment conf in
           let fileblocks = analyse_files e0 !arg_files in
           let fcount = count_files fileblocks in
           let acount = count_assemblies fileblocks in
           Printf.printf "backup of %d files in %d assemblies\n" fcount acount |> ignore;
           run_distributed_backup e0 !arg_nproc acount fileblocks;
           ()
