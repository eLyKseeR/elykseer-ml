open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils
open Mlcpp_chrono



let mk_ok r = match%lwt r with
  | Ok x -> Lwt.return x
  | Error msg -> Alcotest.fail msg

let mk_rel n rel =
  let aid = Printf.sprintf "aid%06d" n in
  let keys : Assembly.keyinformation =
      { pkey = string_of_int (12345678901234567 + n)
      ; ivec = "9876543210123456"
      ; localnchunks=Conversion.i2p 16 } in
  match%lwt Relkeys.add aid keys rel with
  | Ok rel' -> Lwt.return rel'
  | Error msg -> Alcotest.fail msg

let rec prepare_bm cnt rel =
  match cnt with
  | 0 -> Lwt.return rel
  | n -> let%lwt _rel' = mk_rel n rel in
         prepare_bm (n - 1) rel

let check_bm i rel =
  let aid = Printf.sprintf "aid%06d" i in
  let%lwt ks = Relkeys.find aid rel in
  match ks with
  | None -> Lwt.return 0
  | Some _k -> Lwt.return 1

let benchmark_run cnt _ () =
  Printf.printf "benchmarking %d repetitions\n" cnt;
  let config : Configuration.configuration =
    { config_nchunks = Nchunks.from_int 16
    ; path_chunks = "lxr"
    ; path_db = Filename.concat (Filename.get_temp_dir_name ()) "db"
    ; my_id = "4242"
    ; trace  = Tracer.nullTracer } in
  let%lwt rel = Relkeys.new_map config in
  let clock0 = Chrono.Clock.System.now () in
  (* bm1 *)
  let%lwt rel' = prepare_bm cnt rel in
  let clock1 = Chrono.Clock.System.now () in
  (* bm2 *)
  let%lwt () = for%lwt i = 1 to cnt do
    let%lwt nbm = check_bm i rel' in
    Lwt.return (if nbm > 0 then print_string "√" else print_string "x")
  done in
  let clock2 = Chrono.Clock.System.now () in
  let tdiff1 = Chrono.Clock.System.diff clock1 clock0 in
  let tdiff2 = Chrono.Clock.System.diff clock2 clock1 in
  Printf.printf "preparation time:  %s\n" (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff1);
  Printf.printf "verification time: %s\n" (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff2);
  Gc.print_stat stdout;
  Relkeys.close_map rel'

let example_output _ () =
  let config : Configuration.configuration =
    { config_nchunks = Nchunks.from_int 16
    ; path_chunks = "lxr"
    ; path_db = Filename.concat (Filename.get_temp_dir_name ()) "db"
    ; my_id = "4242"
    ; trace = Tracer.nullTracer } in
  let%lwt rel = Relkeys.new_map config in
  let k1 : Assembly.keyinformation = {pkey="key0001";ivec="12";localnchunks=Conversion.i2p 16} in
  let k2 : Assembly.keyinformation = {pkey="key0002";ivec="12";localnchunks=Conversion.i2p 24} in
  let%lwt _ = mk_ok (Relkeys.add "aid001" k1 rel) in
  let%lwt _ = mk_ok (Relkeys.add "aid002" k2 rel) in
  let%lwt () = Relkeys.close_map rel in
  print_endline "done."; Lwt.return ()


(* Runner *)

let test =
  let open Alcotest_lwt in
  "LXR Relkeys",
  [
    test_case "example output" `Quick example_output;
    test_case "benchmark" `Quick (benchmark_run 1000);
  ]