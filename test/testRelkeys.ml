open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils
open Mlcpp_chrono



let mk_rel n rel =
  let aid = Printf.sprintf "aid%06d" n in
  let keys : Assembly.keyinformation =
      { pkey = string_of_int (12345678901234567 + n)
      ; ivec = "9876543210123456"
      ; localnchunks=Conversion.i2p 16; localid="4242" } in
  Relkeys.add aid keys rel

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
  let%lwt () = Lwt_io.printlf "benchmarking %d repetitions" cnt in
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
    if nbm > 0 then Lwt_io.print "√" else Lwt_io.print "x"
  done in
  let clock2 = Chrono.Clock.System.now () in
  let tdiff1 = Chrono.Clock.System.diff clock1 clock0 in
  let tdiff2 = Chrono.Clock.System.diff clock2 clock1 in
  let%lwt () = Lwt_io.printlf "preparation time:  %s" (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff1) in
  let%lwt () = Lwt_io.printlf "verification time: %s" (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff2) in
  Gc.print_stat stdout;
  Lwt.return ()

let example_output _ () =
  let config : Configuration.configuration =
    { config_nchunks = Nchunks.from_int 16
    ; path_chunks = "lxr"
    ; path_db = Filename.concat (Filename.get_temp_dir_name ()) "db"
    ; my_id = "4242"
    ; trace = Tracer.nullTracer } in
  let%lwt rel = Relkeys.new_map config in
  let k1 : Assembly.keyinformation = {pkey="key0001";ivec="12";localnchunks=Conversion.i2p 16;localid="43424"} in
  let k2 : Assembly.keyinformation = {pkey="key0002";ivec="12";localnchunks=Conversion.i2p 24;localid="62831"} in
  let%lwt _ = Relkeys.add "aid001" k1 rel in
  let%lwt _ = Relkeys.add "aid002" k2 rel in
  Lwt_io.printl "done."


(* Runner *)

let test =
  let open Alcotest_lwt in
  "LXR Relkeys",
  [
    test_case "example output" `Quick example_output;
    test_case "benchmark" `Quick (benchmark_run 1000);
  ]