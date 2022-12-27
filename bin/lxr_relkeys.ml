
open Elykseer__Lxr

open Elykseer_utils

open Mlcpp_chrono

let arg_verbose = ref false
let arg_bm = ref false
let arg_fctrl = ref ""

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-b", Arg.Set arg_bm, "benchmark");
    ("-f", Arg.Set_string arg_fctrl, "relation file: file->aid");
  ]

let anon_args_fun _fn = ()

let mk_rel n rel =
  let aid = Printf.sprintf "aid%06d" n in
  let keys : Assembly.keyinformation =
      { pkey = string_of_int (12345678901234567 + n)
      ; ivec = "9876543210123456"
      ; localnchunks=Conversion.i2p 16; localid=Conversion.i2n 4242 } in
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

let benchmark_run cnt =
  let%lwt () = Lwt_io.printlf "benchmarking %d repetitions" cnt in
  let config : Configuration.configuration =
    { config_nchunks = Nchunks.from_int 16
    ; path_chunks = "lxr"
    ; path_db = "/tmp/db"
    ; my_id = Conversion.i2n 4242 } in
  let%lwt rel = Relkeys.new_map config in
  let clock0 = Chrono.Clock.System.now () in
  (* bm1 *)
  let%lwt rel' = prepare_bm cnt rel in
  let clock1 = Chrono.Clock.System.now () in
  (* bm2 *)
  let%lwt () = for%lwt i = 1 to cnt do
    let%lwt nbm = check_bm i rel' in
    if !arg_verbose then 
      if nbm > 0 then Lwt_io.print "√" else Lwt_io.print "x"
    else Lwt.return ()
  done in
  let clock2 = Chrono.Clock.System.now () in
  let tdiff1 = Chrono.Clock.System.diff clock1 clock0 in
  let tdiff2 = Chrono.Clock.System.diff clock2 clock1 in
  let%lwt () = Lwt_io.printlf "preparation time:  %s" (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff1) in
  let%lwt () = Lwt_io.printlf "verification time: %s" (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff2) in
  Gc.print_stat stdout;
  Lwt.return ()

let example_output () =
  let config : Configuration.configuration =
    { config_nchunks = Nchunks.from_int 16
    ; path_chunks = "lxr"
    ; path_db = "/tmp/db"
    ; my_id = Conversion.i2n 4242 } in
  let%lwt rel = Relkeys.new_map config in
  let k1 : Assembly.keyinformation = {pkey="key0001";ivec="12";localnchunks=Conversion.i2p 16;localid=Conversion.i2n 43424} in
  let k2 : Assembly.keyinformation = {pkey="key0002";ivec="12";localnchunks=Conversion.i2p 24;localid=Conversion.i2n 62831} in
  let%lwt _ = Relkeys.add "aid001" k1 rel in
  let%lwt _ = Relkeys.add "aid002" k2 rel in
  Lwt_io.printl "done."

(* main *)
let main () = Arg.parse argspec anon_args_fun "lxr_relkeys: vbf";
  let%lwt () = if !arg_bm
  then
    let%lwt () = benchmark_run 1000 in
    Lwt.return ()
  else if !arg_verbose
    then
      let%lwt () = example_output () in
      Lwt.return ()
    else Lwt.return ()
  in
  Lwt_io.printl "all done."

let () = Lwt_main.run (main ())

(* benchmark 
benchmarking 10000 repetitions        
preparation time:  198 ms
JSON store time: 10 ms
bechmark run time: 172 ms
all done.
compress
encrypt for tester
minor_collections:      1036
major_collections:      4
compactions:            0
forced_major_collections: 0

minor_words:    271160145
promoted_words:    294828
major_words:       302901

top_heap_words: 311296
heap_words:     311296
live_words:     270138
free_words:      40505
largest_free:    40505
fragments:         653

live_blocks: 67747
free_blocks: 1
heap_chunks: 4   
*)