(* [@@@warning "-32"] *)

open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils

module StringMap = Map.Make(String)

let def_myid = "1234567890"

let arg_verbose = ref false
let arg_dryrun = ref false
let arg_files = ref []
let arg_recursive = ref false
let arg_directory = ref ""
let arg_dbpath = ref (Filename.concat (Filename.get_temp_dir_name ()) "db")
let arg_chunkpath = ref "lxr"
let arg_nchunks = ref 16
let arg_myid = ref def_myid

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-y", Arg.Set arg_dryrun, "dry run");
    ("-x", Arg.Set_string arg_chunkpath, "sets output path for encrypted chunks");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
    ("-n", Arg.Set_int arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
    ("-R", Arg.Set arg_recursive, "recursively backup the directory");
    ("-D", Arg.Set_string arg_directory, "directory to backup");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

let output_rel_files config (fistore : Store.FileinformationStore.coq_R) (fbstore : Store.FBlockListStore.coq_R) =
  let fiset = List.map snd fistore.entries in
  let fbis = Env.consolidate_files fbstore.entries in
  if !arg_dryrun then
    Lwt_list.iter_s (fun (fhash, bis) ->
                      let%lwt () = Lwt_io.printlf "fhash = %s" fhash in
                      let%lwt _ = Lwt_list.iter_s (fun (fi : Filesupport.fileinformation) ->
                        Lwt_io.printlf " %s %d %s %d %s %s" fi.fname
                                                       (Conversion.n2i fi.fsize)
                                                       fi.fowner
                                                       (Conversion.n2i fi.fpermissions)
                                                       fi.fmodified
                                                       fi.fchecksum
                        )
                        (fiset) in
                      Lwt_list.iter_s (fun (fb : Assembly.blockinformation) ->
                        Lwt_io.printlf "  %d:%d@%d %s" (Conversion.p2i fb.blockid)
                                                        (Conversion.n2i fb.blocksize)
                                                        (Conversion.n2i fb.filepos)
                                                        fb.bchecksum
                        )
                        (List.rev bis)
                    ) fbis
  else
    let%lwt rel = Relfiles.new_map config in
    let%lwt () = Lwt_list.iter_s (fun (fhash, bis) ->
                                    let fi = List.find (fun fi -> Filesupport.fhash fi = fhash) fiset in
                                    let%lwt _rel' = Relfiles.add fhash {rfi=fi; rfbs=bis} rel in Lwt.return ()) fbis in
    Relfiles.close_map rel

let output_rel_keys config (kstore : Store.KeyListStore.coq_R) =
  let%lwt rel = Relkeys.new_map config in
  let%lwt () = Lwt_list.iter_s (fun (aid, ki) ->
                                let%lwt _ = Relkeys.add aid ki rel in Lwt.return ()) kstore.entries in
  Relkeys.close_map rel

let output_relations (ac : AssemblyCache.assemblycache) =
  let%lwt () = if !arg_dryrun then Lwt.return () else output_rel_keys ac.acconfig ac.ackstore in
  output_rel_files ac.acconfig ac.acfistore ac.acfbstore

let get_file_checksum config filename =
  let map : string StringMap.t = StringMap.empty in
  let%lwt relf = Relfiles.new_map config in
  let fhash = Elykseer_crypto.Sha3_256.string (filename ^ !arg_myid) in
  match%lwt Relfiles.find fhash relf with
  | None -> Lwt.return (map)
  | Some rfbs ->
      (* let%lwt () = if !arg_verbose then
        Lwt_io.printlf "  have info on file '%s' with %d bytes from %d blocks" filename (Conversion.n2i rfbs.rfi.fsize) (List.length rfbs.rfbs)
        else Lwt.return () in *)
      Lwt.return (StringMap.add fhash rfbs.rfi.fchecksum map)

let get_file_blocks config filename =
let map : (Assembly.blockinformation list) StringMap.t = StringMap.empty in
let%lwt relf = Relfiles.new_map config in
let fhash = Elykseer_crypto.Sha3_256.string (filename ^ !arg_myid) in
match%lwt Relfiles.find fhash relf with
| None -> Lwt.return (map)
| Some rfbs ->
    (* let%lwt () = if !arg_verbose then
      Lwt_io.printlf "  have info on file '%s' with %d bytes from %d blocks" filename (Conversion.n2i rfbs.rfi.fsize) (List.length rfbs.rfbs)
      else Lwt.return () in *)
    Lwt.return (StringMap.add fhash rfbs.rfbs map)

let file_meta_maps config filename =
  let%lwt fchecksum_map = get_file_checksum config filename in
  let%lwt fblocks_map = get_file_blocks config filename in
  Lwt.return (fchecksum_map, fblocks_map)

let meta_search_funs fchecksum_map fblocks_map =
  let find_fchecksum = fun fh -> (* Printf.printf "     get fchecksum: %s\n" fh; *) StringMap.find_opt fh fchecksum_map in
  let find_fblocks = fun fh -> (* Printf.printf "     get fblocks: %s\n" fh; *)
    match StringMap.find_opt fh fblocks_map with
    | None -> []
    | Some fbs -> List.rev fbs
  in
  (find_fchecksum, find_fblocks)

let run_file_backup (proc : Processor.processor) filepath =
  let%lwt (fchecksum_map, fblocks_map) = file_meta_maps proc.config (Filesystem.Path.to_string filepath) in
  let (find_fchecksum, find_fblocks) = meta_search_funs fchecksum_map fblocks_map
  in
  Lwt.return (Processor.file_backup proc find_fchecksum find_fblocks filepath)

let run_dir_backup (proc : Processor.processor) dirpath =
  let (lfiles, _) = Processor.list_directory_entries dirpath in
  Lwt_list.fold_left_s (fun proc_i fp -> run_file_backup proc_i fp) proc lfiles


let main () = Arg.parse argspec anon_args_fun "lxr_backup: vyxdnji";
  let nchunks = Nchunks.from_int !arg_nchunks in
  if List.length !arg_files <= 0 && !arg_directory = ""
  then
      let%lwt () = Lwt_io.printl "no directory or no files to backup given in command line arguments." in
      Lwt.return ()
  else
    let myid = !arg_myid in
    let tracer = if !arg_verbose then Tracer.stdoutTracerDebug else Tracer.stdoutTracerWarning in
    let conf : configuration = {
                  config_nchunks = nchunks;
                  path_chunks = !arg_chunkpath;
                  path_db     = !arg_dbpath;
                  my_id       = myid;
                  trace       = tracer } in
    let proc = Processor.prepare_processor conf in
    let%lwt proc' = 
      if !arg_directory = ""
      then
        (* backup each file *)
        Lwt_list.fold_left_s (fun proc_i filename -> run_file_backup proc_i (Filesystem.Path.from_string filename)) proc !arg_files
      else begin
        if !arg_recursive
        then
          (* recursively backup content of directory and its subdirectories *)
          let rec recursive_dir_backup proc_i dirpath =
            let%lwt _ = Lwt_io.printlf "recurse into %s" (Filesystem.Path.to_string dirpath) in
            let%lwt proc1 = run_dir_backup proc_i dirpath in
            let (_, ldirs) = Processor.list_directory_entries dirpath in
            Lwt_list.fold_left_s (fun proc_i' dirname' -> recursive_dir_backup proc_i' dirname') proc1 ldirs in
          recursive_dir_backup proc (Filesystem.Path.from_string !arg_directory)
        else
          (* backup content of directory *)
          run_dir_backup proc (Filesystem.Path.from_string !arg_directory)
      end
    in
    (* close the processor - will extract chunks from current writable environment *)
    let proc'' = Processor.close proc' in
    let%lwt () = output_relations proc''.cache in
    let%lwt () = Lwt_io.printl "done." in
    if !arg_verbose
      then
        let (minw, promw, majw) = Gc.counters () in
        Lwt_io.printlf "    total allocated: %f" (minw +. majw -. promw)
      else Lwt.return ()

let () = Lwt_main.run (main ())
