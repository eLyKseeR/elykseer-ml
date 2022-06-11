
(* open Elykseer *)
open Elykseer__Lxr
open Elykseer__Lxr.BackupPlanner
open Elykseer__Lxr.Configuration
open Elykseer__Lxr.Environment

let arg_verbose = ref false
let arg_files = ref []
let arg_metapath = ref "meta"
let arg_chunkpath = ref "lxr"
let arg_nchunks = ref "16"

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-x", Arg.Set_string arg_chunkpath, "sets output path for encrypted chunks");
    ("-o", Arg.Set_string arg_metapath, "sets output path for meta data");
    ("-n", Arg.Set_string arg_nchunks, "sets number of chunks (16-256) per assembly");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

let rec backup_files fns conf0 e0 =
        match fns with
          [] -> e0
        | fn :: fns' -> let e1 = backup_file conf0 e0 fn in
                        backup_files fns' conf0 e1

(* main *)
let () = Arg.parse argspec anon_args_fun "lxr_backup: vxon";
         let nchunks = int_of_string !arg_nchunks in
         if List.length !arg_files > 0 &&
            nchunks >= 16
         then
           let conf0 : configuration = {
                         num_chunks  = Conversion.i2p nchunks ;
                         path_chunks = !arg_chunkpath;
                         path_meta   = !arg_metapath } in
           let e0 = initial_environment conf0 in
           let e1 = backup_files !arg_files conf0 e0 in
           print_string (Utils.e2s e1)
