(* [@@@warning "-32"] *)

(* open Elykseer *)
open Elykseer__Lxr
open Elykseer__Lxr.Configuration
open Elykseer__Lxr.Environment

open Elykseer_base.Hashing
open Elykseer_utils

open Mlcpp_cstdio

let def_myid = 1234567890

let arg_verbose = ref false
let arg_files = ref []
let arg_metapath = ref "meta"
let arg_dbpath = ref "/tmp/db"
let arg_chunkpath = ref "lxr"
let arg_nchunks = ref 16
let arg_nproc = ref 1
let arg_myid = ref def_myid

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-x", Arg.Set_string arg_chunkpath, "sets output path for encrypted chunks");
    ("-o", Arg.Set_string arg_metapath, "sets output path for meta data");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
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
  let k = Elykseer_crypto.Key256.mk () |> Elykseer_crypto.Key256.to_hex in
  let ki : Assembly.keyinformation =
    { pkey = k
    ; localnchunks = Conversion.i2p @@ !arg_nchunks
    ; localid = Conversion.i2n @@ !arg_myid } in
  let e1 = Environment.env_add_aid_key a.aid e0 ki in
  (* encrypt *)
  match Assembly.encrypt a b ki with
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

let analyse_files e0 fns =
  let a = e0.cur_assembly in
  let afree = curr_assembly_sz a.nchunks - (Conversion.n2i a.apos) in
  let (_anum', _afree', bs) = List.fold_left (fun (anum,afree,acc) fn ->
                                                let afblocks = BackupPlanner.analyse_file a.nchunks afree anum fn in
                                                (afblocks.anum, afblocks.afree, afblocks.ablocks :: acc)
                                             ) (Conversion.i2p 1, Conversion.i2n afree, []) fns in
  let bs' = List.rev bs in
  let fbs = Zip.zip fns bs' in
  (* List.iteri (fun i (fname,fblocks) ->
      Printf.printf "file %d: %s\n" i fname;
      List.iteri (fun j afbs -> Printf.printf "   %d: %d -> %d@%d\n"
                      j (Conversion.p2i @@ BackupPlanner.fbanum afbs) (Conversion.n2i afbs.fbsz) (Conversion.n2i afbs.fbfpos)) fblocks
    ) fbs |> ignore; *)
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
let execute_backup_fileblock e _anum (fname,blocks) =
  (* Printf.printf "    file: %s\n" fname; *)
  (* List.iteri (fun i fb -> Printf.printf "      %d: anum: %d block: %d @ %d\n" i (Conversion.p2i @@ BackupPlanner.fbanum fb) (Conversion.n2i fb.fbsz) (Conversion.n2i fb.fbfpos)) blocks; *)
  Cstdio.File.fopen fname "rx" |> function
    | Ok fptr ->
      let backup_block e0 fptr fb =
          let fpos = BackupPlanner.fbfpos fb in
          Cstdio.File.fseek fptr (Conversion.n2i fpos) |> function
            | Ok _ -> begin
              let sz = Conversion.n2i fb.fbsz in
              let buf = Cstdio.File.Buffer.create sz in
              Cstdio.File.fread buf sz fptr |> function
                | Ok _nread ->
                  let bplain = Buffer.BufferPlain.from_buffer buf in
                  Environment.backup e0 fname fpos bplain
                | Error (errno,errstr) -> 
                  Printf.printf "read error no:%d err:%s\n" errno errstr; e0
              end
            | Error (errno,errstr) -> 
                Printf.printf "seek error no:%d err:%s\n" errno errstr; e0
      in
      let e1 = List.fold_left (fun env fb -> backup_block env fptr fb) e blocks in
      Cstdio.File.fclose fptr |> ignore;
      e1
    | Error (errno,errstr) -> 
        Printf.printf "open error no:%d err:%s\n" errno errstr |> ignore; e

let extract_fileblocks anum fileblocks =
  List.map (fun (fname,bs) -> (fname, List.filter (fun fbs -> BackupPlanner.fbanum fbs = anum) bs)) fileblocks

let rec run_backup_for_assembly e0 pid anum acount fileblocks =
  if anum > acount then e0
  else begin
      (* Printf.printf "executing backup of assembly %d\n" anum; *)
      let anum_p = Conversion.i2p anum in
      let fileblocks' = extract_fileblocks anum_p fileblocks in
      let e1 = List.fold_left (fun env fb -> execute_backup_fileblock env anum fb) e0 fileblocks' in
      let e2 = finalise_and_recreate_assembly e1 in
      run_backup_for_assembly e2 pid (anum + 1) acount fileblocks
  end

(* let start_processes n =
  let rec start_processes' acc n =
    if n = 0
      then List.rev acc
      else
        let proc = n in  (* TODO create external process *)
        start_processes' (proc :: acc) (n - 1) in
  start_processes' [] n *)

let run_distributed_backup e0 nproc acount fileblocks =
  (* start n processes *)
  let ps = (* start_processes *) nproc in
  (* distribute work among processes *)
  run_backup_for_assembly e0 ps 1 acount fileblocks

let output_rel_files e =
  let%lwt rel = Relfiles.new_map e.config in
  let fbis = Env.consolidate_files e.fblocks in
  let%lwt () = Lwt_list.iter_s (fun (fname, bis) -> let fhash = sha256 fname in
                                let%lwt _rel' = Relfiles.add fhash bis rel in Lwt.return ()) fbis in
  Relfiles.close_map rel
  
let output_rel_keys e =
  let%lwt rel = Relkeys.new_map e.config in
  let%lwt () = Lwt_list.iter_s (fun (aid, ki) ->
                                let%lwt _ = Relkeys.add aid ki rel in Lwt.return ()) e.keys in
  Relkeys.close_map rel

let output_relations e =
  let%lwt () = output_rel_keys e in
  output_rel_files e


let main () = Arg.parse argspec anon_args_fun "lxr_backup: vxodnji";
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
                  path_db     = !arg_dbpath;
                  my_id       = Conversion.i2n myid } in
    let e0 = Environment.initial_environment conf in
    let fileblocks = analyse_files e0 !arg_files in
    (* let%lwt () = if !arg_verbose then
      Lwt_list.iter_s (fun (fn,fbs) ->
            Lwt_list.iter_s (fun (fb : BackupPlanner.fileblock) ->
              Lwt_io.printlf " %s@%d %d " fn (Conversion.n2i fb.fbfpos) (Conversion.n2i fb.fbsz)) fbs
      ) fileblocks
    else
      Lwt.return () in *)
    let fcount = count_files fileblocks in
    let acount = count_assemblies fileblocks in
    let%lwt () = Lwt_io.printlf "backup of %d files in %d assemblies" fcount acount in
    let e1 = run_distributed_backup e0 !arg_nproc acount fileblocks in
    let%lwt () = output_relations e1 in
    let%lwt () = Lwt_io.printl "done." in
    Lwt.return ()
  else
    Lwt.return ()

let () = Lwt_main.run (main ())
