[@@@warning "-26-27-32-33-34-35-36-37-38-39-60-66-67"]

(* open Elykseer *)
open Elykseer__Lxr
open Elykseer__Lxr.Configuration
open Elykseer__Lxr.Environment

open Elykseer_utils

open Mlcpp_cstdio

let def_myid = 1234567890

let arg_verbose = ref false
let arg_files = ref []
let arg_metapath = ref "meta"
let arg_chunkpath = ref "lxr"
let arg_nchunks = ref 16
let arg_nproc = ref 1
let arg_myid = ref def_myid

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
(* let aminsz = 127 *)

let finalise_assembly e0 =
  let (a,b) = Assembly.finish (e0.cur_assembly) (e0.cur_buffer) in
  (* create key *)
  let k = Elykseer_crypto.Key256.mk () in
  let e1 = Environment.env_add_aid_key a.aid e0 (Elykseer_crypto.Key256.to_hex k) in
  (* encrypt *)
  match Assembly.encrypt a b (e1.keys) with
  | None -> Printf.printf "failure on encrypting assembly %s\n" a.aid;
            e0
  | Some (a',b') ->
  (* extract to chunks *)
      Assembly.extract e1.config a' b' |> ignore;
      e1

let finalise_and_recreate_assembly e0 =
  let e1 = finalise_assembly e0 in
  Environment.recreate_assembly e1

let curr_assembly_sz n = Conversion.n2i (Assembly.assemblysize n)

let rec analyse_files' acc nchunks afree anum fns =
        match fns with
          [] -> acc
        | fn :: fns' -> let afblocks = BackupPlanner.analyse_file nchunks afree anum fn in
                        analyse_files' (afblocks.ablocks :: acc) nchunks afblocks.afree afblocks.anum fns'
let analyse_files e0 fns =
  let a = e0.cur_assembly in
  let afree = curr_assembly_sz a.nchunks - (Conversion.n2i a.apos) in
  let bs = analyse_files' [] a.nchunks (Conversion.i2n afree) (Conversion.i2p 1) fns in
  let bs' = List.rev bs in
  let fbs = Zip.zip fns bs' in
  List.iteri (fun i (fname,fblocks) ->
      Printf.printf "file %d: %s\n" i fname;
      List.iteri (fun j afbs -> Printf.printf "   %d: %d -> %d@%d\n"
                      j (Conversion.p2i @@ BackupPlanner.fbanum afbs) (Conversion.n2i afbs.fbsz) (Conversion.n2i afbs.fbfpos)) fblocks
    ) fbs |> ignore;
  fbs

let count_files fbs =
  List.map fst fbs |> List.sort_uniq (compare) |> List.length
let count_assemblies fbs =
  List.map snd fbs |> List.flatten |> List.map (fun afbs -> BackupPlanner.fbanum afbs) |> List.sort_uniq (compare) |> List.length
(*
[
  ("tst1.dat", [ (1, 131072,      0); (1, 131072, 131072) ]);
  ("tst2.dat", [ (1, 131072, 917504); (2, 131072,      0) ])
]   
*)

let rec execute_backup_for_assembly e p anum fileblocks =
  match fileblocks with
  | [] -> finalise_and_recreate_assembly e
  | (fname,blocks) :: r ->
    Printf.printf "    file: %s\n" fname;
    List.iteri (fun i fb -> Printf.printf "      %d: anum: %d block: %d @ %d\n" i (Conversion.p2i @@ BackupPlanner.fbanum fb) (Conversion.n2i fb.fbsz) (Conversion.n2i fb.fbfpos)) blocks;
    let e' = Cstdio.File.fopen fname "rx" |> function
      | Ok fptr ->
        let rec backup_block e0 fptr blocks =
          match blocks with
          | [] -> e0
          | fb :: rbs ->
            let fpos = BackupPlanner.fbfpos fb in
            Cstdio.File.fseek fptr (Conversion.n2i fpos) |> function
              | Ok _ -> begin
                let sz = Conversion.n2i fb.fbsz in
                let buf = Cstdio.File.Buffer.create sz in
                Cstdio.File.fread buf sz fptr |> function
                  | Ok _nread ->
                    let bplain = Buffer.BufferPlain.from_buffer buf in
                    let e1 = Environment.backup e0 fname fpos bplain in
                    backup_block e1 fptr rbs
                  | Error (errno,errstr) -> 
                      Printf.printf "read error no:%d err:%s\n" errno errstr; e0
                end
              | Error (errno,errstr) -> 
                  Printf.printf "seek error no:%d err:%s\n" errno errstr; e0
        in
        let e1 = backup_block e fptr blocks in
        Cstdio.File.fclose fptr |> ignore;
        e1
      | Error (errno,errstr) -> 
          Printf.printf "open error no:%d err:%s\n" errno errstr |> ignore; e
    in
    execute_backup_for_assembly e' p anum r

let extract_fileblocks anum fileblocks =
  List.map (fun (fname,bs) -> (fname, List.filter (fun fbs -> BackupPlanner.fbanum fbs = anum) bs)) fileblocks

let rec run_backup_for_assembly e ps anum acount fileblocks =
  if anum > acount then e
  else
    match ps with
    | [] -> e
    | p :: ps' -> 
      Printf.printf "executing backup of assembly %d\n" anum;
      let anum_p = Conversion.i2p anum in
      let fileblocks' = extract_fileblocks anum_p fileblocks in
      let e' = execute_backup_for_assembly e p anum_p fileblocks' in
      run_backup_for_assembly e' (List.append ps' [p]) (anum + 1) acount fileblocks

let start_processes n =
  let rec start_processes' acc n =
    if n = 0
      then List.rev acc
      else
        let proc = n in  (* TODO create external process *)
        start_processes' (proc :: acc) (n - 1) in
  start_processes' [] n

let run_distributed_backup e0 nproc acount fileblocks =
  (* start n processes *)
  let ps = start_processes nproc in
  (* distribute work among processes *)
  run_backup_for_assembly e0 ps 1 acount fileblocks

let output_relations e =
  let dt_id = Elykseer_base.Assembly.date_ident () in
  let meta_p = e.config.path_meta in
  Relfiles.save_to_file e.files (Mlcpp_filesystem.Filesystem.Path.from_string (meta_p ^ "/" ^ dt_id ^ ".relfiles")) "tester" |> ignore;
  Relkeys.save_to_file e.keys (Mlcpp_filesystem.Filesystem.Path.from_string (meta_p ^ "/" ^ dt_id ^ ".relkeys")) "tester" |> ignore

(* main *)
let () = Arg.parse argspec anon_args_fun "lxr_backup: vxonji";
         let nchunks = Nchunks.from_int !arg_nchunks in
         if List.length !arg_files > 0
          && !arg_nproc > 0 && !arg_nproc < 65
         then
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
           let e1 = run_distributed_backup e0 !arg_nproc acount fileblocks in
           output_relations e1;
           ()
