
let arg_myid = ref Elykseer_utils.Cli.default_myid

let usage_msg = "lxr_filehash -f filepath [-i myid]"

let arg_fp = ref ""

let argspec =
  [
    ("-f", Arg.Set_string arg_fp, "file path");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
  ]

let anon_args_fun _fn = ()

let main () = Arg.parse argspec anon_args_fun usage_msg;
  Elykseer_utils.Cli.require argspec usage_msg [("-f", !arg_fp)];
  Lwt_io.printl ("file=" ^ !arg_fp ^ " " ^ (Elykseer_crypto.Sha3_256.string (!arg_fp ^ !arg_myid)))

let () = Lwt_main.run (main ())
