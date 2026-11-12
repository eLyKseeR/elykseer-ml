
open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils

open Mlcpp_filesystem

let arg_verbose = ref false
let arg_files = ref []
let arg_dbpath = ref ""
let arg_chunkpath = ref ""
let arg_outpath = ref (Filename.get_temp_dir_name ())
let arg_myid = ref Cli.default_myid
let arg_verify = ref false

let usage_msg = "lxr_restore -x chunkpath -d dbpath [-o outpath] [-v] [-i myid] <file1> [<file2>] ...\n       lxr_restore --verify -x chunkpath -d dbpath [-v] [-i myid] [<file1> ...]"

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-x", Arg.Set_string arg_chunkpath, "sets path for encrypted chunks");
    ("-o", Arg.Set_string arg_outpath, "sets output path for restored files (default: temp directory)");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
    ("-n", Arg.Int (fun _ -> prerr_endline "lxr_restore: parameter -n is obsolete and ignored, the number of chunks is read from the key information"), "obsolete, ignored");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
    ("--verify", Arg.Set arg_verify, "restores into a temporary directory, compares each file with its checksum from the meta data, and removes it; without file names: all files of the identifier");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

(* the restored file must stay below the output directory, also when
   following symlinks that already exist in it *)
let contained_target basep fname =
  match Pathutils.restore_relpath fname with
  | Error msg -> Error msg
  | Ok relp ->
    let targetp = Filesystem.Path.append basep (Filesystem.Path.from_string relp) in
    match Filesystem.Path.weakly_canonical basep, Filesystem.Path.weakly_canonical targetp with
    | Some cbase, Some ctarget
      when Pathutils.is_below ~base:(Filesystem.Path.to_string cbase) (Filesystem.Path.to_string ctarget) ->
        Ok (Filesystem.Path.from_string relp, targetp)
    | _ -> Error (Printf.sprintf "file name '%s' resolves outside of the output directory" fname)

(* restores a file and returns (bytes restored, success, processor);
   a file that could not be completely restored is removed again *)
let restore_file proc basep (fname, ofbs) =
  match ofbs with
  | None -> let%lwt () = Lwt_io.printlf "  cannot restore file '%s': no meta data found" fname in Lwt.return (0,false,proc)
  | Some (rfbs : Relfiles.relation) ->
    match contained_target basep fname with
    | Error msg -> let%lwt () = Lwt_io.printlf "  cannot restore file '%s': %s" fname msg in Lwt.return (0,false,proc)
    | Ok (relp, targetp) ->
      let target = Filesystem.Path.to_string targetp in
      if Filesystem.Path.exists targetp then
        let%lwt () = Lwt_io.printlf "  cannot restore file '%s': '%s' already exists" fname target in
        Lwt.return (0,false,proc)
      else
        match Processor.file_restore proc basep relp rfbs.rfbs with
        | exception Elykseer_base.Assembly.Content_error msg ->
          if Sys.file_exists target then Sys.remove target;
          let%lwt () = Lwt_io.printlf "  failed to restore file '%s': %s" fname msg in
          Lwt.return (0,false,proc)
        | (n, proc') ->
        let n = Conversion.n2i n
        and fsize = Conversion.n2i rfbs.rfi.fsize in
        if n = fsize && !arg_verify then begin
          (* the blocks were verified already, this checks the file as a whole *)
          let ok = Elykseer_crypto.Sha3_256.file target = rfbs.rfi.fchecksum in
          Sys.remove target;
          let%lwt () = if not ok then Lwt_io.printlf "  failed to verify file '%s': checksum differs" fname
                       else if !arg_verbose then Lwt_io.printlf "  verified file '%s'" fname
                       else Lwt.return () in
          Lwt.return ((if ok then n else 0), ok, proc')
        end
        else if n = fsize then
          Lwt.return (n,true,proc')
        else begin
          (* missing blocks: e.g. lost chunks, wrong key, or corrupted data *)
          if Sys.file_exists target then Sys.remove target;
          let%lwt () = Lwt_io.printlf "  failed to restore file '%s': restored %d of %d bytes" fname n fsize in
          Lwt.return (0,false,proc')
        end

(* find all assembly ids in the file blocks to be restored
   and put their encryption keys into the key store of the
   assembly cache *)
let ensure_keys_available (ac0 : AssemblyCache.assemblycache) relk rels =
  let laids = List.fold_left (fun acc (_, ofbs) ->
                match ofbs with
                | None -> acc
                | Some (fbs : Relfiles.relation) ->
                    List.append (List.map (fun (bi : Assembly.blockinformation) -> bi.blockaid) fbs.rfbs) acc
              ) [] rels in
  let lsorted = List.sort_uniq (compare) laids in
  let%lwt kstore' = Lwt_list.fold_left_s (fun kstore aid ->
                      match%lwt Relkeys.find aid relk with
                      | None -> Lwt.return kstore
                      | Some ki -> Lwt.return (Store.KeyListStore.add aid ki kstore)
                    ) ac0.ackstore lsorted
                    in
  Lwt.return { ac0 with ackstore = kstore' }

(* returns the number of files that failed to restore *)
let restore_files (proc0 : Processor.processor) relf relk basep fns =
    match fns with
    | [] -> Lwt.return 0
    | _  -> (* the meta data of each file is read only once *)
            let%lwt rels = Lwt_list.map_s (fun fn ->
                             let%lwt ofbs = Relfiles.find (Elykseer_crypto.Sha3_256.string (fn ^ !arg_myid)) relf in
                             Lwt.return (fn, ofbs)) fns in
            let%lwt ac' = ensure_keys_available proc0.cache relk rels in
            let proc1 = { proc0 with cache = ac'} in
              let nf = List.length fns in
              let%lwt (cnt,nok,_proc') = Lwt_list.fold_left_s (fun (c,k,proc) fn ->
                                       let%lwt (c',ok,proc') = restore_file proc basep fn in
                                       Lwt.return(c + c', (if ok then k + 1 else k), proc')
                                     ) (0,0,proc1) rels in
              let nfailed = nf - nok in
              let%lwt () = if !arg_verbose || nfailed > 0 then
                Lwt_io.printlf "  %s %d of %d files with %d bytes in total" (if !arg_verify then "verified" else "restored") nok nf cnt
                else Lwt.return () in
              Lwt.return nfailed

let exists_output_dir d =
  let dp = Filesystem.Path.from_string d in
  if Filesystem.Path.exists dp && Filesystem.Path.is_directory dp
    then true
    else begin
      Printf.printf "output directory '%s' does not exist or is not a directory\n" d;
      false
    end

let main () = Arg.parse argspec anon_args_fun usage_msg;
    let () = if !arg_files <> [] || !arg_verify then begin
        Cli.require argspec usage_msg [("-x", !arg_chunkpath); ("-d", !arg_dbpath)];
        Cli.require_db !arg_dbpath;
        Cli.warn_default_myid !arg_myid end in
    if (!arg_verify || List.length !arg_files > 0)
       && (!arg_verify || exists_output_dir !arg_outpath)
    then
      let myid = !arg_myid in
      let tracer = if !arg_verbose then Tracer.stdoutTracerDebug else Tracer.stdoutTracerWarning in
      let conf : configuration = {
                    (* restore reads the number of chunks from the key information *)
                    config_nchunks = Nchunks.from_int Cli.min_nchunks;
                    path_chunks = !arg_chunkpath;
                    path_db     = !arg_dbpath;
                    my_id       = myid;
                    trace       = tracer } in
      let proc = Processor.prepare_processor conf in
      let%lwt relf = Relfiles.new_map conf in
      let%lwt relk = Relkeys.new_map conf in
      if !arg_verify then
        let tmpdir = Filename.temp_dir "lxr_verify" "" in
        let%lwt fns = match !arg_files with
          | [] -> (* all files of this identifier *)
            let%lwt hs = Relfiles.hashes relf in
            Lwt_list.filter_map_s (fun h ->
                match%lwt Relfiles.find h relf with
                | Some r -> Lwt.return (Some r.rfi.fname)
                | None -> Lwt.return None) hs
          | fns -> Lwt.return fns in
        let%lwt nfailed = restore_files proc relf relk (Filesystem.Path.from_string tmpdir) fns in
        ignore (Sys.command (Filename.quote_command "rm" ["-rf"; tmpdir]));
        Lwt.return nfailed
      else
      let basep = Filesystem.Path.from_string !arg_outpath in
      restore_files proc relf relk basep !arg_files
    else
      let%lwt () = Lwt_io.printl "nothing to do." in
      Lwt.return 0

let () = if Lwt_main.run (main ()) > 0 then exit 1
