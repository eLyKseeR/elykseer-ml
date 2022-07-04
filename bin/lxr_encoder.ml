(* open Lwt *)

(* open Elykseer__Lxr *)
open Elykseer__Lxr.Environment

open Elykseer_utils

let arg_verbose = ref false
let arg_fctrl = ref ""

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-f", Arg.Set_string arg_fctrl, "control file for LXR encoder");
  ]

let anon_args_fun _fn = ()


(* main *)
let main () = Arg.parse argspec anon_args_fun "lxr_encoder: vf";
    let _setup = setup_environment in
    let e = Envutils.envrestore !arg_fctrl in
    Lwt_io.printl (Utils.e2s e)

let () = Lwt_main.run (main ())
