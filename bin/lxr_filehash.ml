
open Elykseer_base.Hashing

let arg_fp = ref ""

let argspec =
  [
    ("-f", Arg.Set_string arg_fp, "file path");
  ]

let anon_args_fun _fn = ()

let main () = Arg.parse argspec anon_args_fun "lxr_filehash: f";
  if !arg_fp != ""
  then Lwt_io.printl (sha256 !arg_fp) 
  else Lwt_io.printl "nothing."

let () = Lwt_main.run (main ())
