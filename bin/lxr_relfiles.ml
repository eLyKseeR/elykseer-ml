
open Elykseer__Lxr
open Elykseer__Lxr.RelationFileAid

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

let rec mk_blocks i aid apos fpos =
  match i with
  | 0 -> []
  | n ->
    let blen = 3247 in
    let block = { blockid = Conversion.i2p n; bchecksum = "chk";
                  blocksize = Conversion.i2n blen;
                  filepos = Conversion.i2n fpos;
                  blockaid = aid; blockapos = Conversion.i2n apos } in
    block :: mk_blocks (n - 1) aid (apos + blen) (fpos + blen)
let mk_rel n aid rel =
  let rnd = Elykseer_crypto.Random.with_rng (fun rng -> Elykseer_crypto.Random.r32_range rng 1 12) in
  let blocks = mk_blocks rnd aid 1200 0 in
  let fname = Printf.sprintf "test_%04d.dat" n in
  RelationFileAid.add fname blocks rel

let rec prepare_bm cnt rel =
  match cnt with
  | 0 -> rel
  | n -> let rel' = mk_rel n "aid00012345789" rel in
         prepare_bm (n - 1) rel'

let run_bm i rel =
  let fname = Printf.sprintf "test_%04d.dat" i in
  let blocksopt = RelationFileAid.find fname rel in
  match blocksopt with
  | None -> 0
  | Some blocks ->
    List.fold_left (fun acc b -> Conversion.n2i(b.blocksize) + acc) 0 blocks

let benchmark_run cnt =
  let%lwt () = Lwt_io.printlf "benchmarking %d repetitions" cnt in
  let rel = RelationFileAid.coq_new in
  let clock0 = Chrono.Clock.System.now () in
  (* bm1 *)
  let rel' = prepare_bm cnt rel in
  let clock1 = Chrono.Clock.System.now () in
  (* bm2 *)
  Relfiles.save_to_file rel' (Filesystem.Path.from_string "./meta/bm.json") "tester" |> ignore;
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
  let rel = RelationFileAid.coq_new in
  let blocks1 = [ { blockid = Conversion.i2p 1; blockaid = "aid001";
                    filepos = Conversion.i2n 0; blocksize = Conversion.i2n 1200;
                    bchecksum = "check1"; blockapos = Conversion.i2n 37001
                  }
                ; { blockid = Conversion.i2p 2; blockaid = "aid001";
                    filepos = Conversion.i2n 1200; blocksize = Conversion.i2n 670;
                    bchecksum = "check2"; blockapos = Conversion.i2n (37001+1200)
                  } ] in
  let blocks2 = [ { blockid = Conversion.i2p 1; blockaid = "aid001";
                    filepos = Conversion.i2n 0; blocksize = Conversion.i2n 1200;
                    bchecksum = "check1"; blockapos = Conversion.i2n (37001+1200+670)
                  }
                ; { blockid = Conversion.i2p 2; blockaid = "aid001";
                    filepos = Conversion.i2n 1200; blocksize = Conversion.i2n 860;
                    bchecksum = "check2"; blockapos = Conversion.i2n (37001+1200+670+1200)
                  }
                ; { blockid = Conversion.i2p 3; blockaid = "aid002";
                    filepos = Conversion.i2n (1200+860); blocksize = Conversion.i2n 323;
                    bchecksum = "check3"; blockapos = Conversion.i2n 0
                  } ] in
  let rel' = RelationFileAid.add "testfile01.data" blocks1 rel |>
             RelationFileAid.add "testfile02.data" blocks2 in
  Relfiles.save_to_file rel' (Filesystem.Path.from_string "test101.json") "testuser" |> ignore;
  let (_bsz, b) = Relfiles.output_to_buffer rel' in
  Lwt_io.printlf "%s" (Cstdio.File.Buffer.to_string b)

let example_input () =
  let t0 = Chrono.Clock.System.now () in
  Relfiles.load_from_file (Filesystem.Path.from_string "./meta/test101.json") "testuser" |> function
  | None -> Lwt_io.printl "could not load relation"
  | Some rel -> let l = RelationFileAid.M.elements rel in
    let t1 = Chrono.Clock.System.now () in
    let tdiff = Chrono.Clock.System.diff t1 t0 in
    Lwt_io.printlf "loaded %d entries in %s"
      (List.length l) (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff)
(* result: 
loaded 10000 entries in 502 ms   *)

(* main *)
let main () = Arg.parse argspec anon_args_fun "lxr_relfiles: vbf";
  let%lwt () = if !arg_bm
  then
    let%lwt () = benchmark_run 10000 in
    (* let%lwt () = benchmark_run 1000 in
    let%lwt () = benchmark_run 10000 in *)
    (* let%lwt () = benchmark_run 100000 in *)
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


(** benchmark runs *)

(** 1) with list as the underlying type
    
benchmarking 100 repetitions        
preparation time:  0 ms
bechmark run time: 8 ms
benchmarking 1000 repetitions
preparation time:  8 ms
bechmark run time: 663 ms
benchmarking 10000 repetitions
preparation time:  114 ms
bechmark run time: 57438 ms

*)

(** 2) with AVL trees as underlying type

benchmarking 100 repetitions        
preparation time:  2 ms
bechmark run time: 2 ms
benchmarking 1000 repetitions
preparation time:  27 ms
bechmark run time: 14 ms
benchmarking 10000 repetitions
preparation time:  289 ms
bechmark run time: 195 ms

*)

(* benchmarking JSON store times 

benchmarking 10000 repetitions       
preparation time:  256 ms
JSON store time: 80288 ms
bechmark run time: 2591 ms
all done.
compress
encrypt for tester
minor_collections:      17336
major_collections:      2798
compactions:            0
forced_major_collections: 1

minor_words:    313080002
promoted_words:   5778878
major_words:      5786951

top_heap_words: 6201856
heap_words:     6201856
live_words:     5426072
free_words:      775269
largest_free:    742115
fragments:          515

live_blocks: 2480159
free_blocks: 3295
heap_chunks: 25

-rw-------  1 alex  staff  3839111 17 Oct 10:57 bm.json

benchmarking 1000 repetitions        
preparation time:  29 ms
JSON store time: 651 ms
bechmark run time: 31 ms
all done.
compress
encrypt for tester
minor_collections:      1501
major_collections:      245
compactions:            0
forced_major_collections: 0

minor_words:    25534145
promoted_words:   631232
major_words:      639305

top_heap_words: 661504
heap_words:     661504
live_words:     605718
free_words:      55678
largest_free:    53285
fragments:         108

live_blocks: 255227
free_blocks: 291
heap_chunks: 9

-rw-------  1 alex  staff  383268 17 Oct 11:22 bm.json
*)

(* before changing to new Cstdio.File.Buffer.resize

benchmarking 10000 repetitions       
preparation time:  298 ms
JSON store time: 54783 ms
bechmark run time: 260 ms
all done.
reallocs: 107793
compress
encrypt for tester
minor_collections:      1192
major_collections:      9
compactions:            0
forced_major_collections: 1

minor_words:    311646532
promoted_words:   5503498
major_words:      5511571

top_heap_words: 6201856
heap_words:     6201856
live_words:     5483518
free_words:      717701
largest_free:    717701
fragments:          637

live_blocks: 2491462
free_blocks: 1
heap_chunks: 25
*)

(* after changing to Cstdio.File.Buffer.resize

benchmarking 10000 repetitions       
preparation time:  295 ms
JSON store time: 84 ms
bechmark run time: 193 ms
all done.
reallocs: 0
compress
encrypt for tester
minor_collections:      1191
major_collections:      9
compactions:            0
forced_major_collections: 1

minor_words:    311368069
promoted_words:   5524349
major_words:      5532422

top_heap_words: 6201856
heap_words:     6201856
live_words:     5503765
free_words:      697423
largest_free:    697423
fragments:          668

live_blocks: 2500777
free_blocks: 1
heap_chunks: 25

-rw-------  1 alex  staff  3936697 19 Oct 20:23 bm.json

*)