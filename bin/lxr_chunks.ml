
(* open Elykseer *)
open Elykseer__Lxr
open Elykseer__Lxr.Configuration
(* open Elykseer__Lxr.Environment *)

(* open Elykseer_utils *)

(* open Mlcpp_cstdio
open Mlcpp_filesystem *)

let def_myid = "1234567890"

let arg_verbose = ref false
let arg_files = ref []
let arg_aid = ref "<tbd>"
let arg_chunkpath = ref "lxr"
let arg_nchunks = ref 16
let arg_myid = ref def_myid

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-a", Arg.Set_string arg_aid, "sets assembly id");
    ("-x", Arg.Set_string arg_chunkpath, "sets path for encrypted chunks");
    ("-n", Arg.Set_int arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

(* main *)
let () = Arg.parse argspec anon_args_fun "lxr_chunks: vxni";
         let nchunks = Nchunks.from_int !arg_nchunks in
         let myid = !arg_myid in
         let conf : configuration = {
                         config_nchunks = nchunks;
                         path_chunks = !arg_chunkpath;
                         path_db     = "/tmp/db";
                         my_id       = myid;
                         trace       = Tracer.stdoutTracer } in
         List.iter (fun cid -> Assembly.chunk_identifier_path conf !arg_aid cid |> Printf.printf "%03d %s\n" (Conversion.p2i cid))
            @@ Utilities.make_list (Nchunks.to_positive nchunks)