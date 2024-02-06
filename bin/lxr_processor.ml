(* [@@@warning "-32"] *)

(*
  example of a file backup implementation
  using Processor module from Coq

  usage: lxr_processor <options> <filenames>

  lxr_processor: vxdni
    -v verbose output
    -x sets output path for encrypted chunks
    -d sets database path
    -n sets number of chunks (16-256) per assembly
    -i sets own identifier
    -help  Display this list of options
    --help  Display this list of options

*)

open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils

let def_myid = "1234567890"

let arg_verbose = ref false
let arg_files = ref []
let arg_dbpath = ref "/tmp/db"
let arg_chunkpath = ref "lxr"
let arg_nchunks = ref 16
let arg_myid = ref def_myid

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-x", Arg.Set_string arg_chunkpath, "sets output path for encrypted chunks");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
    ("-n", Arg.Set_int arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

let output_relfiles proc fis =
  let fbis = Env.consolidate_files (Processor.get_fblocks proc).entries in
  let%lwt rel = Relfiles.new_map (Processor.config proc) in
  let%lwt () = Lwt_list.iter_s (fun (fhash, bis) ->
                                  let fi = List.find (fun (fi : Filesupport.fileinformation) -> Elykseer_crypto.Sha256.string fi.fname = fhash) fis in
                                  let%lwt _rel' = Relfiles.add fhash {rfi=fi; rfbs=bis} rel in Lwt.return ())
                               fbis in
  Relfiles.close_map rel

let output_relkeys proc =
  let%lwt rel = Relkeys.new_map (Processor.config proc) in
  let%lwt () = Lwt_list.iter_s (fun (aid, ki) ->
                                let%lwt _ = Relkeys.add aid ki rel in Lwt.return ()) (Processor.get_keys proc).entries in
  Relkeys.close_map rel
  
let main () = Arg.parse argspec anon_args_fun "lxr_processor: vxdni";
  let nchunks = Nchunks.from_int !arg_nchunks in
  if List.length !arg_files <= 0
  then
    let%lwt () = Lwt_io.printl "no files to backup given in command line arguments." in
    Lwt.return ()
  else
    let myid = !arg_myid in
    let conf : configuration = {
                  config_nchunks = nchunks;
                  path_chunks = !arg_chunkpath;
                  path_db     = !arg_dbpath;
                  my_id       = myid } in

    (* backup each file *)
    let (proc0, fis) = List.fold_left (fun (proc, fis) filename -> let (fi, (_, proc')) = Processor.file_backup proc filename in (proc', fi :: fis))
                                      ((Processor.prepare_processor conf), []) !arg_files in
    (* close the processor - will extract chunks from current writable environment *)
    let proc1 = Processor.close proc0 in

    (* access meta data and output to irmin database *)
    let%lwt () = output_relkeys proc1 in
    let%lwt () = output_relfiles proc1 fis in

    (* output the collected file informations when in verbose mode *)
    let%lwt () = if !arg_verbose then
        Lwt_list.iter_s (fun (fi : Filesupport.fileinformation) ->
            Lwt_io.printlf "file = %s\n  size = %d\n  owner = %s\n  permissions = %d\n  modified = %s\n  checksum = %s"
                          (fi.fname) (Conversion.n2i fi.fsize) (fi.fowner)
                          (Conversion.n2i fi.fpermissions) (fi.fmodified) (fi.fchecksum)) fis
      else Lwt.return () in

    Lwt.return ()

let () = Lwt_main.run (main ())


(*
example run:

% MYID="testtest"
% dune exec lxr_processor -- -n 16 -v -i ${MYID} /bin/sh /bin/tcsh

> file = /bin/sh
>   size = 134000
>   owner = 0
>   permissions = 755
>   modified = 2024-01-11 11:39:45
>   checksum = b612f129d876f6eeb9aa2cdb14deed3101fd24a2f7e2874a3c8e24b1a8bc3a2b
> file = /bin/tcsh
>   size = 1153408
>   owner = 0
>   permissions = 755
>   modified = 2024-01-11 11:39:45
>   checksum = 0a8b5e09b9beed0811c014d808b4e05c49ff73c8fa072e9ccc6ad47a7f4b5b34

(meta data are stored in irmin db at default location: /tmp/db)
(chunks are output to default location: ./lxr)

% dune exec lxr_restore -- -v -o /tmp/ -n 16 -i ${MYID} /bin/sh

>   restoring 134000 bytes in file '/bin/sh' from 5 blocks
> +✅ '/bin/sh'    restored with 134000 bytes in total
>   restored 1 files with 134000 bytes in total

% ls -l /tmp/bin/sh

> -rw-r--r--@ 1 user  wheel  134000 Feb  4 19:41 /tmp/bin/sh

*)