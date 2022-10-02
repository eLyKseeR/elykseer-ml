
(* open Elykseer *)
open Elykseer__Lxr
open Elykseer__Lxr.Configuration

let def_myid = 1234567890

let arg_verbose = ref false
let arg_files = ref []
let arg_metapath = ref "meta"
let arg_chunkpath = ref "lxr"
let arg_nchunks = ref "16"
let arg_myid = ref (string_of_int def_myid) (* "1234567890" *)

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-x", Arg.Set_string arg_chunkpath, "sets path for encrypted chunks");
    ("-f", Arg.Set_string arg_metapath, "sets path for meta data");
    ("-n", Arg.Set_string arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

let rec backup_files conf0 e0 fns =
        match fns with
          [] -> e0
        | fn :: fns' -> let e1 = backup_file conf0 e0 fn in
                        backup_files conf0 e1 fns'

(* main *)
let () = Arg.parse argspec anon_args_fun "lxr_restore: vxfni";
         let nchunks = int_of_string !arg_nchunks in
         if List.length !arg_files > 0 &&
            nchunks >= 16 && nchunks <= 256
         then
           let _setup = setup_environment in
           let myid = let id0 = int_of_string !arg_myid in
                      if id0 >= 0 then id0 else def_myid in
           let conf0 : configuration = {
                         num_chunks  = Conversion.i2p nchunks ;
                         path_chunks = !arg_chunkpath;
                         path_meta   = !arg_metapath;
                         my_id       = Conversion.i2n myid } in
           let e0 = initial_environment conf0 in
           let e1 = restore_file conf0 e0 !arg_files in
           print_string @@ Elykseer_utils.Utils.e2j e1;
