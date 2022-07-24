open Lwt

open Elykseer__Lxr
open Elykseer__Lxr.Environment

open Elykseer_utils

let arg_verbose = ref false
let arg_fctrl = ref ""
let arg_nproc = ref 1

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-f", Arg.Set_string arg_fctrl, "control file for LXR encoder");
    ("-j", Arg.Set_int arg_nproc, "sets number of parallel processes");
  ]

let anon_args_fun _fn = ()

(* let lwt_print abs =
    Lwt_list.iter_s (fun (anum,fbs) -> 
        Lwt_io.printf "%d:\n" (Conversion.p2i anum) |> ignore;
        Lwt_list.iter_s (fun fb -> Lwt_io.printl(Utils.fibs2s fb)) fbs
    ) abs *)

let run abs =
    if !arg_verbose
    then List.map (fun (anum,fbs) -> print_string("anum = "); print_int(Conversion.p2i anum); print_endline("");
             List.map (fun fb -> print_endline @@ Utils.fibs2s fb) fbs
         ) abs |> ignore
    else ()
    ;
    let pool = Lwt_pool.create !arg_nproc
                  ~dispose: (fun p -> if !arg_verbose then print_endline("dispose process.") else ();
                                      p#close >|= ignore)
                  (fun () ->
                      if !arg_verbose then print_endline("starting process.") else ();
                      Lwt_process.open_process("", [|"echo"; "calling lxr_encoder"|]) |> Lwt.return) in
    
    let processor i (_idx,fibs) p =
      (if !arg_verbose then Lwt_io.printf "  process %d:\n  assembly: %s\n" i (String.concat "; " (List.map Utils.fibs2s fibs)) else Lwt.return ()) |> ignore;
      Lwt_io.read_lines p#stdout |>
      Lwt_stream.iter_s Lwt_io.printl in
    
    let mklist n =
      let rec loop k l =
        if k <= 0 then l else loop (k - 1) (k :: l) in
      loop n [] in

    mklist (List.length abs) |>
      Lwt_stream.of_list |>
      Lwt_stream.iter_p (fun i -> Lwt_pool.use pool (processor i (List.nth abs (i - 1))))

(* main *)
let main () = Arg.parse argspec anon_args_fun "lxr_controller: vfj";
    let _setup = setup_environment in
    let e = Envutils.envrestore !arg_fctrl in
    let abs = Envutils.env2assemblies e in
    (* lwt_print abs *)
    run abs

let () = Lwt_main.run (main ())
