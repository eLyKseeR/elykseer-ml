(* open Lwt *)

(* open Elykseer__Lxr *)
open Elykseer__Lxr.Assembly
(* open Elykseer__Lxr.Block *)
open Elykseer__Lxr.Environment

open Elykseer_utils

let arg_verbose = ref false
let arg_fctrl = ref ""
let arg_aid = ref ""

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-a", Arg.Set_string arg_aid, "id of assembly");
    ("-f", Arg.Set_string arg_fctrl, "control file for LXR encoder");
  ]

let anon_args_fun _fn = ()

let assembly_check e p_aid : assembly option Lwt.t =
    Lwt.return @@ List.find_opt (fun a -> aid a = p_aid) e.assemblies

(* main *)
let main () = Arg.parse argspec anon_args_fun "lxr_assemlby: vaf";
    let _setup = setup_environment in
    let e = Envutils.envrestore !arg_fctrl in
    let%lwt () = Lwt_io.printl (Utils.e2s e) in
    let%lwt a = assembly_check e !arg_aid in
    match a with
    | None -> Lwt_io.printl "nothing found"
    | Some a0 ->
        let a1 = restore (e.config) a0 in
        Lwt_io.printl (Utils.as2s a1)

let () = Lwt_main.run (main ())
