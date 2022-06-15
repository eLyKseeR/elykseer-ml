
(* open Elykseer__Lxr *)
(* open Elykseer__Lxr.Configuration *)
open Elykseer__Lxr.Environment

open Elykseer_utils

let arg_verbose = ref false
let arg_fctrl = ref ""
let arg_nproc = ref "1"

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-f", Arg.Set_string arg_fctrl, "control file for LXR encoder");
    ("-j", Arg.Set_string arg_nproc, "sets number of parallel processes");
  ]

let anon_args_fun _fn = ()

(* main *)
let () = Arg.parse argspec anon_args_fun "lxr_encoder: vfj";
         let _setup = setup_environment in
         let fctrl = !arg_fctrl in
 
         let e = Envutils.envrestore fctrl in

         print_string @@ Elykseer_utils.Utils.e2j e;
           (* Utilities.rnd (Conversion.i2n 1) |> Conversion.n2i |> print_int *)
