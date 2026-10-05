(* [@@@warning "-32"] *)

open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils

module StringMap = Map.Make(String)

let arg_verbose = ref false
let arg_dryrun = ref false
let arg_files = ref []
let arg_recursive = ref false
let arg_directory = ref ""
let arg_dbpath = ref ""
let arg_chunkpath = ref ""
let arg_nchunks = ref 16
let arg_myid = ref Cli.default_myid

let usage_msg = "lxr_backup -x chunkpath -d dbpath [-v] [-y] [-n nchunks] [-i myid] [-D directory [-R]] [<file1> ...]"

(* number of failures: files that could not be backed up, or inconsistent meta data *)
let failures = ref 0
let fail fmt = Printf.ksprintf (fun m -> incr failures; prerr_endline m) fmt

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-y", Arg.Set arg_dryrun, "dry run");
    ("-x", Arg.Set_string arg_chunkpath, "sets output path for encrypted chunks");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
    ("-n", Cli.nchunks_spec arg_nchunks, "sets number of chunks (16-256) per assembly");
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
                                    match List.find_opt (fun fi -> Filesupport.fhash fi = fhash) fiset with
                                    | None -> (* blocks of a file whose backup failed *)
                                        fail "no file information for blocks of file hash %s; meta data not written" fhash;
                                        Lwt.return ()
                                    | Some fi ->
                                        match%lwt Relfiles.add fhash {rfi=fi; rfbs=bis} rel with
                                        | Ok _ -> Lwt.return ()
                                        | Error msg -> fail "%s" msg; Lwt.return ()) fbis in
    Relfiles.close_map rel

let output_rel_keys config (kstore : Store.KeyListStore.coq_R) =
  let%lwt rel = Relkeys.new_map config in
  let%lwt () = Lwt_list.iter_s (fun (aid, ki) ->
                                match%lwt Relkeys.add aid ki rel with
                                | Ok _ -> Lwt.return ()
                                | Error msg -> fail "%s" msg; Lwt.return ()) kstore.entries in
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

(* a file that cannot be read is reported and skipped *)
let readable_file fp =
  match Unix.stat fp with
  | exception Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)
  | st when st.Unix.st_kind <> Unix.S_REG -> Error "not a regular file"
  | _ -> (match Unix.access fp [Unix.R_OK] with
          | () -> Ok ()
          | exception Unix.Unix_error (e, _, _) -> Error (Unix.error_message e))

let run_file_backup (proc : Processor.processor) filepath =
  match readable_file (Filesystem.Path.to_string filepath) with
  | Error msg ->
    fail "cannot backup file '%s': %s" (Filesystem.Path.to_string filepath) msg;
    Lwt.return proc
  | Ok () ->
  let%lwt (fchecksum_map, fblocks_map) = file_meta_maps proc.config (Filesystem.Path.to_string filepath) in
  let (find_fchecksum, find_fblocks) = meta_search_funs fchecksum_map fblocks_map
  in
  match Processor.file_backup proc find_fchecksum find_fblocks filepath with
  | proc' -> Lwt.return proc'
  | exception Elykseer_base.Assembly.Content_error msg ->
    (* the processor before this file is still consistent: its apos was not advanced *)
    fail "cannot backup file '%s': %s" (Filesystem.Path.to_string filepath) msg;
    Lwt.return proc

let run_dir_backup (proc : Processor.processor) dirpath =
  let (lfiles, _) = Processor.list_directory_entries dirpath in
  Lwt_list.fold_left_s (fun proc_i fp -> run_file_backup proc_i fp) proc lfiles


(* every block must refer to an assembly whose key is known, either from
   this run or from the meta data of an earlier backup (deduplication) *)
let check_keys config (ac : AssemblyCache.assemblycache) =
  let known = List.map fst ac.ackstore.entries in
  let aids = List.map (fun (_, (bi : Assembly.blockinformation)) -> bi.blockaid) ac.acfbstore.entries
             |> List.sort_uniq compare
             |> List.filter (fun aid -> not (List.mem aid known)) in
  match aids with
  | [] -> Lwt.return ()
  | _ ->
    let%lwt relk = Relkeys.new_map config in
    Lwt_list.iter_s (fun aid ->
        match%lwt Relkeys.find aid relk with
        | Some _ -> Lwt.return ()
        | None -> fail "no key for assembly %s; its blocks cannot be restored" aid; Lwt.return ()
      ) aids

let main () = Arg.parse argspec anon_args_fun usage_msg;
  let nchunks = Nchunks.from_int !arg_nchunks in
  if List.length !arg_files <= 0 && !arg_directory = ""
  then
      let%lwt () = Lwt_io.printl "no directory or no files to backup given in command line arguments." in
      Lwt.return 0
  else
    let () = Cli.require argspec usage_msg [("-x", !arg_chunkpath); ("-d", !arg_dbpath)] in
    let () = Cli.warn_default_myid !arg_myid in
    let () = if not !arg_dryrun then Cli.note_new_db !arg_dbpath in
    let myid = !arg_myid in
    let (tracer, nwarnings) = Tracing.counting (if !arg_verbose then Tracer.stdoutTracerDebug else Tracer.stdoutTracerWarning) in
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
    let%lwt () = check_keys conf proc''.cache in
    let nfailed = !failures + nwarnings () in
    let%lwt () = if nfailed > 0
      then Lwt_io.eprintlf "backup incomplete: %d failure%s, see messages above" nfailed (if nfailed > 1 then "s" else "")
      else Lwt_io.printl "done." in
    let%lwt () = if !arg_verbose
      then
        let (minw, promw, majw) = Gc.counters () in
        Lwt_io.printlf "    total allocated: %f" (minw +. majw -. promw)
      else Lwt.return () in
    Lwt.return nfailed

let () = if Lwt_main.run (main ()) > 0 then exit 1
