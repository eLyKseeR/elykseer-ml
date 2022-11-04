
(* open Elykseer *)
open Elykseer__Lxr
open Elykseer__Lxr.Configuration
(* open Elykseer__Lxr.Environment *)

open Elykseer_utils

(* open Mlcpp_cstdio *)
open Mlcpp_filesystem

let def_myid = 1234567890

let arg_verbose = ref false
let arg_files = ref []
let arg_metapath = ref "meta"
let arg_chunkpath = ref "lxr"
let arg_nchunks = ref 16
let arg_nproc = ref 1
let arg_ref1 = ref ""
let arg_ref2 = ref ""
let arg_ref3 = ref ""
let arg_myid = ref def_myid

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-x", Arg.Set_string arg_chunkpath, "sets path for encrypted chunks");
    ("-f", Arg.Set_string arg_metapath, "sets path for meta data");
    ("-n", Arg.Set_int arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-j", Arg.Set_int arg_nproc, "sets number of parallel processes");
    ("-r1", Arg.Set_string arg_ref1, "adds reference files .relkeys and .relfiles");
    ("-r2", Arg.Set_string arg_ref2, "adds reference files .relkeys and .relfiles");
    ("-r3", Arg.Set_string arg_ref3, "adds reference files .relkeys and .relfiles");
    ("-i", Arg.Set_int arg_myid, "sets own identifier (positive number)");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

let restore_file e0 _fn =
  Printf.printf "  restoring file '%s'\n" _fn;
  e0

let rec restore_files e0 fns =
  match fns with
    [] -> ()
  | fn :: fns' -> let e1 = restore_file e0 fn in
                  restore_files e1 fns'

let merge_rel_files e0 fn =
  match Relfiles.load_from_file (Filesystem.Path.from_string fn) "testuser" (* TODO *) with
  | None -> e0
  | Some map ->
    List.fold_left (fun e (fname,bis) ->
      List.fold_left (Environment.env_add_file_block fname) e bis)
      e0 (RelationFileAid.M.elements map)

let merge_rel_keys e0 fn =
  match Relkeys.load_from_file (Filesystem.Path.from_string fn) "testuser" (* TODO *) with
  | None -> e0
  | Some map ->
    List.fold_left (fun e (aid,ki) -> Environment.env_add_aid_key aid e ki)
      e0 (RelationAidKey.M.elements map)

let load_reference e0 fbase =
  let e1 = merge_rel_keys e0 (!arg_metapath ^ "/" ^ fbase ^ ".relkeys") in
  let e2 = merge_rel_files e1 (!arg_metapath ^ "/" ^ fbase ^ ".relfiles") in
  Printf.printf "   number of files: %d\n" (List.length (RelationFileAid.M.elements e2.files));
  Printf.printf "   number of keys: %d\n" (List.length (RelationAidKey.M.elements e2.keys));
  e2

let load_references e0 = 
  let e1 = if !arg_ref1 = "" then e0
           else load_reference e0 !arg_ref1 in
  let e2 = if !arg_ref2 = "" then e1
           else load_reference e1 !arg_ref2 in
  let e3 = if !arg_ref3 = "" then e2
           else load_reference e2 !arg_ref3 in
  e3

(* main *)
let () = Arg.parse argspec anon_args_fun "lxr_restore: vxfnji";
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
                         my_id       = Conversion.i2n myid } in
           let e0 = Environment.initial_environment conf in
           let e1 = load_references e0 in
           restore_files e1 !arg_files
