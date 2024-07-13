
open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils

open Mlcpp_filesystem

let def_myid = "1234567890"

let arg_verbose = ref false
let arg_files = ref []
let arg_dbpath = ref "/tmp/db"
let arg_chunkpath = ref "lxr"
let arg_outpath = ref "/tmp/"
let arg_nchunks = ref 16
let arg_myid = ref def_myid

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-x", Arg.Set_string arg_chunkpath, "sets path for encrypted chunks");
    ("-o", Arg.Set_string arg_outpath, "sets output path for restored files");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
    ("-n", Arg.Set_int arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

let restore_file proc relf _relk basep fname =
  let%lwt ofbs = Relfiles.find (Elykseer_crypto.Sha3_256.string (fname ^ !arg_myid)) relf in
  match ofbs with
  | None -> let%lwt () = Lwt_io.printlf "  cannot restore file '%s'" fname in Lwt.return (0,proc)
  | Some rfbs ->
      (* let%lwt () = if !arg_verbose then
        Lwt_io.printlf "  restoring %d bytes in file '%s' from %d blocks" (Conversion.n2i rfbs.rfi.fsize) fname (List.length rfbs.rfbs)
        else Lwt.return () in *)
      let (n, proc') = Processor.file_restore proc basep (Filesystem.Path.from_string fname) rfbs.rfbs in
      (* let%lwt _ = Lwt_io.printlf "     -> %d bytes" (Conversion.n2i n) in *)
      Lwt.return (Conversion.n2i n, proc')

(* find all assembly ids in the file blocks to be restored
   and put their encryption keys into the key store of the
   assembly cache *)
let ensure_keys_available (ac0 : AssemblyCache.assemblycache) relf relk fns =
  let%lwt laids = Lwt_list.fold_left_s (fun acc fname ->
                    let fhash = Elykseer_crypto.Sha3_256.string (fname ^ !arg_myid) in
                    match%lwt Relfiles.find fhash relf with
                    | None -> Lwt.return acc
                    | Some fbs ->
                        let laids = List.map (fun (bi : Assembly.blockinformation) -> bi.blockaid) fbs.rfbs in
                        Lwt.return (List.append laids acc)
                  ) [] fns
                  in
  let lsorted = List.sort_uniq (compare) laids in
  let%lwt kstore' = Lwt_list.fold_left_s (fun kstore aid ->
                      match%lwt Relkeys.find aid relk with
                      | None -> Lwt.return kstore
                      | Some ki -> Lwt.return (Store.KeyListStore.add aid ki kstore)
                    ) ac0.ackstore lsorted
                    in
  Lwt.return { ac0 with ackstore = kstore' }

let restore_files (proc0 : Processor.processor) relf relk basep fns =
    match fns with
    | [] -> Lwt.return ()
    | _  -> let%lwt ac' = ensure_keys_available proc0.cache relf relk fns in
            let proc1 = { proc0 with cache = ac'} in
              let nf = List.length fns in
              let%lwt (cnt,_proc') = Lwt_list.fold_left_s (fun (c,proc) fn ->
                                       let%lwt (c',proc') = restore_file proc relf relk basep fn in
                                       Lwt.return(c + c',proc')
                                     ) (0,proc1) fns in
              let%lwt () = if !arg_verbose then
                Lwt_io.printlf "  restored %d files with %d bytes in total" nf cnt
                else Lwt.return () in
              Lwt.return ()

let exists_output_dir d =
  let dp = Filesystem.Path.from_string d in
  if Filesystem.Path.exists dp && Filesystem.Path.is_directory dp
    then true
    else begin
      Printf.printf "output directory '%s' does not exist or is not a directory\n" d;
      false
    end

let main () = Arg.parse argspec anon_args_fun "lxr_restore: vxodnji";
    let nchunks = Nchunks.from_int !arg_nchunks in
    if List.length !arg_files > 0
       && exists_output_dir !arg_outpath
    then
      let myid = !arg_myid in
      let tracer = if !arg_verbose then Tracer.stdoutTracerDebug else Tracer.stdoutTracerWarning in
      let conf : configuration = {
                    config_nchunks = nchunks;
                    path_chunks = !arg_chunkpath;
                    path_db     = !arg_dbpath;
                    my_id       = myid;
                    trace       = tracer } in
      let proc = Processor.prepare_processor conf in
      let%lwt relf = Relfiles.new_map conf in
      let%lwt relk = Relkeys.new_map conf in
      let basep = Filesystem.Path.from_string !arg_outpath in
      restore_files proc relf relk basep !arg_files
    else
      Lwt_io.printl "nothing to do."

let () = Lwt_main.run (main ())
