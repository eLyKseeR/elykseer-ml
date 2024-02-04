
open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_base.Fsutils
open Elykseer_base.Hashing
open Elykseer_utils

open Mlcpp_cstdio
open Mlcpp_filesystem

let def_myid = "1234567890"

let arg_verbose = ref false
let arg_files = ref []
let arg_dbpath = ref "/tmp/db"
let arg_chunkpath = ref "lxr"
let arg_outpath = ref "/tmp/"
let arg_nchunks = ref 16
let arg_nproc = ref 1
let arg_myid = ref def_myid

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-x", Arg.Set_string arg_chunkpath, "sets path for encrypted chunks");
    ("-o", Arg.Set_string arg_outpath, "sets output path for restored files");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
    ("-n", Arg.Set_int arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-j", Arg.Set_int arg_nproc, "sets number of parallel processes");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

let mk_path fp =
  !arg_outpath ^ "/" ^ fp

let ensure_assembly e relk aid =
  if (Environment.cur_assembly e |> Assembly.aid) = aid
    then Lwt.return @@ Ok (Environment.cur_assembly e, Environment.cur_buffer e)
  else
  match%lwt Relkeys.find aid relk with
  | None -> Lwt.return @@ Error "no key found"
  | Some ki ->
    let e1 = Environment.EnvironmentReadable.env_add_aid_key aid e ki in
    match Environment.EnvironmentReadable.restore_assembly e1 aid with
    | None -> Lwt.return @@ Error "failed to restore assembly"
    | Some e2 -> Lwt.return @@ Ok (e2.cur_assembly, e2.cur_buffer)

let restore_file_blocks e0 relk fptr (fb : Assembly.blockinformation) =
  match%lwt ensure_assembly e0 relk fb.blockaid with
  | Error errstr -> let%lwt () = Lwt_io.printlf "  failed to recall assembly %s with '%s'" fb.blockaid errstr in
                    Lwt.return (0,e0)
  | Ok (a,b) ->
      let sz = Conversion.n2i fb.blocksize in
      let fbuf = Cstdio.File.Buffer.create sz in
      let abuf = Elykseer__Lxr.Cstdio.BufferPlain.to_buffer @@ Assembly.id_buffer_t_from_full b in
      let nread = Elykseer_base.Assembly.get_content ~src:abuf ~sz:sz ~pos:(Conversion.n2i fb.blockapos) ~tgt:fbuf in
      Cstdio.File.fseek fptr (Conversion.n2i fb.filepos) |> function
        | Error _ -> let%lwt () = Lwt_io.printlf "  failed to 'fseek'\n" in Lwt.return (0,e0)
        | Ok _ -> Cstdio.File.fwrite fbuf sz fptr |> function
          | Error _ -> let%lwt () = Lwt_io.printlf "  failed to 'fwrite'\n" in Lwt.return (0,e0)
          | Ok _nwritten -> Lwt.return (nread,{ e0 with cur_assembly=a; cur_buffer=b })

let restore_file e0 relf relk fname =
  let%lwt ofbs = Relfiles.find (sha256 fname) relf in
  match ofbs with
  | None -> let%lwt () = Lwt_io.printlf "  cannot restore file '%s'" fname in Lwt.return (0,e0)
  | Some rfbs ->
      let%lwt () = if !arg_verbose then
        Lwt_io.printlf "  restoring %d bytes in file '%s' from %d blocks" (Conversion.n2i rfbs.rfi.fsize) fname (List.length rfbs.rfbs)
        else Lwt.return () in
      let fout_path = mk_path fname in
      let dir_path = Filesystem.Path.from_string fout_path |> Filesystem.Path.parent in
      let%lwt () = if not (Filesystem.Path.exists dir_path) then
          Filesystem.create_directories dir_path |> function
          | false -> let%lwt () = Lwt_io.printlf "failed to create directories: %s" fout_path in
            Lwt.return ()
          | true -> Lwt.return ()
        else Lwt.return () in
      Cstdio.File.fopen fout_path "wx" |> function
        | Error (errno,errstr) -> let%lwt () = Lwt_io.printf "  fopen returned: %d/%s\n    (filepath '%s')" errno errstr fout_path in Lwt.return (0,e0)
        | Ok fptr ->
            let%lwt (cnt,e1) = Lwt_list.fold_left_s (fun (cnt,env) fb -> let%lwt (c',e') = restore_file_blocks env relk fptr fb in Lwt.return(cnt + c',e')) (0,e0) rfbs.rfbs in
            let () = Cstdio.File.fclose fptr |> ignore in
            let (res,res') = if rfbs.rfi.fchecksum = fchksum fout_path then ("+","✅") else ("-","❌") in
            let%lwt () = Lwt_io.printf "%s%s '%s'" res res' fname in
            let%lwt () = if !arg_verbose then
                Lwt_io.printlf "    restored with %d bytes in total" cnt
              else
                Lwt_io.printl ""
              in
            Lwt.return (cnt,e1)

let ensure_all_available (e : Environment.EnvironmentReadable.coq_E) fns =
  let%lwt rel = Relfiles.new_map e.config in
  let%lwt ls = Lwt_list.map_s (fun fname -> let fhash = sha256 fname in
                  match%lwt Relfiles.find fhash rel with None -> Lwt.return 0 | Some _ -> Lwt.return 1) fns in
  Lwt.return @@ ((List.fold_left ((+)) 0 ls) == List.length fns)

let restore_files e0 relf relk fns =
    match fns with
    | [] -> Lwt.return ()
    | _ -> if%lwt ensure_all_available e0 fns then
              let nf = List.length fns in
              let%lwt (cnt,_e) = Lwt_list.fold_left_s (fun (c,e) fn -> let%lwt (c',e') = restore_file e relf relk fn in Lwt.return(c + c',e')) (0,e0) fns in
              let%lwt () = if !arg_verbose then
                Lwt_io.printlf "  restored %d files with %d bytes in total" nf cnt
                else Lwt.return () in
              Lwt.return ()
           else Lwt_io.printf "information on some files not found!"

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
    && !arg_nproc > 0 && !arg_nproc < 65
    && exists_output_dir !arg_outpath
    then
      let myid = !arg_myid in
      let conf : configuration = {
                    config_nchunks = nchunks;
                    path_chunks = !arg_chunkpath;
                    path_db     = !arg_dbpath;
                    my_id       = myid } in
      let e0 = Environment.EnvironmentReadable.initial_environment conf in
      let%lwt relf = Relfiles.new_map conf in
      let%lwt relk = Relkeys.new_map conf in
      restore_files e0 relf relk !arg_files
    else Lwt.return ()

let () = Lwt_main.run (main ())
