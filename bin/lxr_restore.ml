
open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils

open Mlcpp_filesystem

let def_myid = "1234567890"

let arg_verbose = ref false
let arg_files = ref []
let arg_dbpath = ref (Filename.concat (Filename.get_temp_dir_name ()) "db")
let arg_chunkpath = ref "lxr"
let arg_outpath = ref (Filename.get_temp_dir_name ())
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
let restore_file proc relf _relk basep fname =
  let%lwt ofbs = Relfiles.find (Elykseer_crypto.Sha3_256.string (fname ^ !arg_myid)) relf in
  match ofbs with
  | None -> let%lwt () = Lwt_io.printlf "  cannot restore file '%s': no meta data found" fname in Lwt.return (0,false,proc)
  | Some rfbs ->
    match contained_target basep fname with
    | Error msg -> let%lwt () = Lwt_io.printlf "  cannot restore file '%s': %s" fname msg in Lwt.return (0,false,proc)
    | Ok (relp, targetp) ->
      let target = Filesystem.Path.to_string targetp in
      if Filesystem.Path.exists targetp then
        let%lwt () = Lwt_io.printlf "  cannot restore file '%s': '%s' already exists" fname target in
        Lwt.return (0,false,proc)
      else
        let (n, proc') = Processor.file_restore proc basep relp rfbs.rfbs in
        let n = Conversion.n2i n
        and fsize = Conversion.n2i rfbs.rfi.fsize in
        if n = fsize then
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

(* returns the number of files that failed to restore *)
let restore_files (proc0 : Processor.processor) relf relk basep fns =
    match fns with
    | [] -> Lwt.return 0
    | _  -> let%lwt ac' = ensure_keys_available proc0.cache relf relk fns in
            let proc1 = { proc0 with cache = ac'} in
              let nf = List.length fns in
              let%lwt (cnt,nok,_proc') = Lwt_list.fold_left_s (fun (c,k,proc) fn ->
                                       let%lwt (c',ok,proc') = restore_file proc relf relk basep fn in
                                       Lwt.return(c + c', (if ok then k + 1 else k), proc')
                                     ) (0,0,proc1) fns in
              let nfailed = nf - nok in
              let%lwt () = if !arg_verbose || nfailed > 0 then
                Lwt_io.printlf "  restored %d of %d files with %d bytes in total" nok nf cnt
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
      let%lwt () = Lwt_io.printl "nothing to do." in
      Lwt.return 0

let () = if Lwt_main.run (main ()) > 0 then exit 1
