
let def_myid = "1234567890"

let arg_myid = ref def_myid

let arg_fp = ref ""

let argspec =
  [
    ("-f", Arg.Set_string arg_fp, "file path");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
  ]

let anon_args_fun _fn = ()

let main () = Arg.parse argspec anon_args_fun "lxr_filehash: fi";
  if !arg_fp != ""
  then Lwt_io.printl ("file=" ^ !arg_fp ^ " " ^ (Elykseer_crypto.Sha256.string (!arg_fp ^ !arg_myid)))
  else Lwt_io.printl "nothing."

let () = Lwt_main.run (main ())
