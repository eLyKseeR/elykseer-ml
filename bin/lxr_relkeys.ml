
open Elykseer__Lxr
(* open Elykseer__Lxr.RelationAidKey *)

open Elykseer_utils

open Mlcpp_chrono
open Mlcpp_cstdio
open Mlcpp_filesystem

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
  let keys : RelationAidKey.keyinformation =
      { pkey="01234567890123456789012345678901"
      ; localnchunks=Conversion.i2p 16; localid=Conversion.i2n 4242 } in
  RelationAidKey.add aid keys rel

let rec prepare_bm cnt rel =
  match cnt with
  | 0 -> rel
  | n -> let rel' = mk_rel n  rel in
         prepare_bm (n - 1) rel'

let run_bm i rel =
  let aid = Printf.sprintf "aid%06d" i in
  let ks = RelationAidKey.find aid rel in
  match ks with
  | None -> 0
  | Some _k -> 1

let benchmark_run cnt =
  let%lwt () = Lwt_io.printlf "benchmarking %d repetitions" cnt in
  let rel = RelationAidKey.coq_new in
  let clock0 = Chrono.Clock.System.now () in
  (* bm1 *)
  let rel' = prepare_bm cnt rel in
  let clock1 = Chrono.Clock.System.now () in
  (* bm2 *)
  Relkeys.save_to_file rel' (Filesystem.Path.from_string "./meta/bm_keys.json") "tester" |> ignore;
  let clock2 = Chrono.Clock.System.now () in
  (* bm3 *)
  for i = 1 to cnt do
    let bm = run_bm i rel' in
    if !arg_verbose then 
      if bm > 0 then print_string "√" else print_string "x"
     else ()
  done;
  let clock3 = Chrono.Clock.System.now () in
  let tdiff1 = Chrono.Clock.System.diff clock1 clock0 in
  let tdiff2 = Chrono.Clock.System.diff clock2 clock1 in
  let tdiff3 = Chrono.Clock.System.diff clock3 clock2 in
  let%lwt () = Lwt_io.printlf "preparation time:  %s" (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff1) in
  let%lwt () = Lwt_io.printlf "JSON store time: %s" (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff2) in
  let%lwt () = Lwt_io.printlf "bechmark run time: %s" (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff3) in
  Gc.print_stat stdout;
  Lwt.return ()

let example_output () =
  let rel = RelationAidKey.coq_new in
  let k1 : RelationAidKey.keyinformation = {pkey="key0001";localnchunks=Conversion.i2p 16;localid=Conversion.i2n 43424} in
  let k2 : RelationAidKey.keyinformation = {pkey="key0002";localnchunks=Conversion.i2p 24;localid=Conversion.i2n 62831} in
  let rel' = RelationAidKey.add "aid001" k1 rel |>
             RelationAidKey.add "aid002" k2 in
  Relkeys.save_to_file rel' (Filesystem.Path.from_string "testkeys.json") "testuser" |> ignore;
  let (_bsz, b) = Relkeys.output_to_buffer rel' in
  Lwt_io.printlf "%s" (Cstdio.File.Buffer.to_string b)

let example_input () =
  let t0 = Chrono.Clock.System.now () in
  Relkeys.load_from_file (Filesystem.Path.from_string "./meta/testkeys.json") "testuser" |> function
  | None -> Lwt_io.printl "could not load relation"
  | Some rel -> let l = RelationAidKey.M.elements rel in
    let t1 = Chrono.Clock.System.now () in
    let tdiff = Chrono.Clock.System.diff t1 t0 in
    Lwt_io.printlf "loaded %d entries in %s"
      (List.length l) (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff)
(* result: 
loaded 10000 entries in 216 ms   *)

(* main *)
let main () = Arg.parse argspec anon_args_fun "lxr_relkeys: vbf";
  let%lwt () = if !arg_bm
  then
    let%lwt () = benchmark_run 10000 in
    Lwt.return ()
  else if !arg_verbose
    then
      let%lwt () = example_output () in
      let%lwt () = example_input () in
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