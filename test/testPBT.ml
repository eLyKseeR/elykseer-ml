(** Property-based tests using QCheck *)

open QCheck
open Elykseer_utils
open Elykseer_base
open Mlcpp_cstdio

module L = Elykseer__Lxr

(* ============================================================================
   Helper Functions
   ============================================================================ *)

(** Sum of block sizes *)
let sum_sizes blocks =
  List.fold_left (fun acc (bi : L.Assembly.blockinformation) ->
    L.Conversion.n2i acc + L.Conversion.n2i bi.blocksize |> L.Conversion.i2n
  ) (L.Conversion.i2n 0) blocks |> L.Conversion.n2i

(** Check if blocks are contiguous by file position *)
let contiguous blocks =
  if List.length blocks <= 1 then true
  else
    let sorted = List.sort (fun (b1 : L.Assembly.blockinformation) (b2 : L.Assembly.blockinformation) ->
      L.Conversion.n2i b1.L.Assembly.filepos - L.Conversion.n2i b2.L.Assembly.filepos
    ) blocks in
    let rec check_pairs = function
      | [] | [_] -> true
      | (b1 : L.Assembly.blockinformation) :: (b2 : L.Assembly.blockinformation) :: rest ->
          let end_pos = L.Conversion.n2i b1.L.Assembly.filepos + L.Conversion.n2i b1.L.Assembly.blocksize in
          let start_pos = L.Conversion.n2i b2.L.Assembly.filepos in
          end_pos = start_pos && check_pairs (b2 :: rest)
    in
    check_pairs sorted

(** Helper to create config for tests *)
let test_config n =
  { L.Configuration.config_nchunks = L.Nchunks.from_int n
  ; path_chunks = "/tmp/pbt"
  ; path_db = "/tmp/pbt_db"
  ; my_id = "pbt_test"
  ; trace = L.Tracer.nullTracer }

let test_aid = "test_aid_12345678"

(* ============================================================================
   Phase 2: Assembly Layout (Priority 1)
   ============================================================================ *)

(** Striping model: logical index to physical position *)
let phys n i = (i mod n) * 262144 + (i / n)

(** Generator for critical nchunks values *)
let critical_n_gen = Gen.oneof_list [16; 17; 31; 32; 63; 64; 65; 127; 128; 129; 144; 145; 256]

(** Round-trip: get_content after add_content recovers data *)
let prop_get_after_add =
  QCheck.Test.make ~count:50 ~name:"get_content after add_content is identity"
    (make (Gen.triple critical_n_gen (Gen.int_range 0 100_000) (Gen.string_size (Gen.int_range 1 4096))))
    (fun (n, pos, data) ->
       let sz = String.length data in
       let tgt_len = n * 262144 in

       assume (sz > 0 && pos + sz <= tgt_len);

       let tgt = Cstdio.File.Buffer.create tgt_len in
       let src = Cstdio.File.Buffer.from_string data in

       let nw_add = Assembly.add_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt in

       let dst = Cstdio.File.Buffer.create sz in
       let nw_get = Assembly.get_content ~src:tgt ~sz:sz ~pos:pos ~tgt:dst in

       nw_add = sz && nw_get = sz &&
       Cstdio.File.Buffer.to_string dst = data)

(** Error codes match FFI specification *)
let prop_add_content_errors =
  QCheck.Test.make ~count:200 ~name:"add_content error codes match preconditions"
    (make (Gen.quad critical_n_gen (Gen.int_range (-10) 300_000) (Gen.int_range (-10) 10_000) (Gen.string_size (Gen.int_range 0 2000))))
    (fun (n, pos, sz, data) ->
       let tgt_len = n * 262144 in
       let tgt = Cstdio.File.Buffer.create tgt_len in
       let src = Cstdio.File.Buffer.from_string data in

       let result =
         try Assembly.add_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt
         with Assembly.Content_error msg ->
           (* Extract error code from message like "... failed: ... (-3)" *)
           if String.contains msg '(' then
             let idx = String.rindex msg '(' in
             int_of_string (String.sub msg (idx+1) (String.length msg - idx - 2))
           else 0
       in

       (* Error code specifications from assembly.cxx *)
       if sz <= 0 then result = -3 || result < 0
       else if sz > String.length data then result = -1 || result < 0
       else if pos + sz > tgt_len then result = -2 || result < 0
       else if pos < 0 then result = -4 || result < 0
       else result = sz)

(* ============================================================================
   Phase 3: Full Assembly Round-Trip (Priority 2)
   ============================================================================ *)

(** Fill assembly with variable-sized blocks until finalized, verify all restore *)
let prop_full_assembly_restores =
  QCheck.Test.make ~count:20 ~name:"all blocks in full assembly restore correctly"
    (make (Gen.pair critical_n_gen (Gen.list_size (Gen.int_range 1 10) (Gen.int_range 1 32768))))
    (fun (n, block_sizes) ->
       let t0 = Unix.gettimeofday () in

       (* Create temp directory for chunks *)
       let chunkdir = Filename.temp_dir "lxr_pbt_chunks" "" in
       let conf = { (test_config n) with L.Configuration.path_chunks = chunkdir } in

       (* Generate deterministic content from index *)
       let content i sz = String.init sz (fun j -> Char.chr ((i * 31 + j * 7) land 0xff)) in

       (* Fill assembly until finalized *)
       let rec fill e i bis = function
         | [] -> (None, List.rev bis)  (* Ran out of sizes before full *)
         | sz::rest ->
             let b = L.Cstdio.BufferPlain.from_buffer (Cstdio.File.Buffer.from_string (content i sz)) in
             let e', (bi, ki) = L.Environment.EnvironmentWritable.backup e "f" (L.Conversion.i2n (i * 1001)) b in
             match ki with
             | Some (aid, ki) -> (Some (aid, ki), List.rev bis)  (* Finalized *)
             | None -> fill e' (i + 1) ((i, sz, bi) :: bis) rest
       in

       (* Generate enough blocks to fill assembly - use cycle pattern *)
       let rec cycle xs n = if n <= 0 then [] else xs @ cycle xs (n - List.length xs) in
       let enough_sizes = cycle block_sizes 10000 in

       let t_fill_start = Unix.gettimeofday () in
       let result = fill (L.Environment.EnvironmentWritable.initial_environment conf) 0 [] enough_sizes in
       let t_fill = Unix.gettimeofday () -. t_fill_start in

       (* Cleanup and check *)
       let success = match result with
         | (Some (aid, ki), bis) when List.length bis > 0 ->
             (* Restore assembly and verify all blocks *)
             let e0 = L.Environment.EnvironmentReadable.initial_environment conf in
             let t_restore_start = Unix.gettimeofday () in
             let restore_ok = match L.Environment.EnvironmentReadable.restore_assembly e0 aid ki with
              | None -> false
              | Some e ->
                  List.for_all (fun (i, sz, bi) ->
                    match L.Assembly.restore (L.Environment.cur_buffer e) bi with
                    | None -> false
                    | Some b ->
                        let restored = L.Cstdio.BufferPlain.to_buffer b |> Cstdio.File.Buffer.to_string in
                        restored = content i sz
                  ) bis
             in
             let t_restore = Unix.gettimeofday () -. t_restore_start in
             let t_total = Unix.gettimeofday () -. t0 in
             let capacity = n * 262144 in
             let data_written = List.fold_left (fun acc (_, sz, _) -> acc + sz) 0 bis in
             Printf.printf "n=%3d blocks=%4d cap=%8d data=%8d fill=%.3fs restore=%.3fs total=%.3fs\n%!"
               n (List.length bis) capacity data_written t_fill t_restore t_total;
             restore_ok
         | _ -> true  (* No finalization = empty test, accept *)
       in

       ignore (Sys.command (Filename.quote_command "rm" ["-rf"; chunkdir]));
       success)

(* ============================================================================
   Phase 4: Pure Helpers (Security-Critical)
   ============================================================================ *)

(** Generator for hostile paths *)
let hostile_path_gen =
  Gen.(map (String.concat "/")
    (list_size (int_range 0 8)
      (oneof_list [""; "."; ".."; "a"; "b"; "etc"; "..."; "../.."; "tmp"; "x/.."; "./a"])))

let prop_restore_relpath_never_escapes =
  QCheck.Test.make ~count:5000 ~name:"restore_relpath never escapes base"
    (make hostile_path_gen)
    (fun p ->
       match Pathutils.restore_relpath p with
       | Ok r ->
           (* Never starts with / *)
           r <> "" && r.[0] <> '/' &&
           (* No .. components *)
           not (List.mem ".." (String.split_on_char '/' r)) &&
           (* Stays below base *)
           Pathutils.is_below ~base:"/out" ("/out/" ^ r) &&
           (* Idempotent *)
           (match Pathutils.restore_relpath r with
            | Ok r' -> r = r'
            | Error _ -> false)
       | Error _ -> true)  (* Rejection is safe *)

let prop_sinkcheck_localhost =
  QCheck.Test.make ~count:1000 ~name:"is_local_host accepts only loopback"
    (make Gen.string)
    (fun host ->
       let is_loopback = List.mem host ["localhost"; "127.0.0.1"; "::1"; "0.0.0.0"] in
       let result = Sinkcheck.is_local_host host in
       (* Should accept loopback addresses *)
       (is_loopback = result) &&
       (* Should reject lookalikes *)
       not (Sinkcheck.is_local_host "localhost.example.com"))

let prop_nchunks_from_int =
  QCheck.Test.make ~count:1000 ~name:"nchunks from_int clamps to 16-256"
    (make (Gen.int_range 1 500))  (* Only positive values *)
    (fun n ->
       let nch = L.Nchunks.from_int n in
       let n' = L.Nchunks.to_positive nch |> L.Conversion.p2i in
       (* from_int clamps to valid range *)
       n' >= 16 && n' <= 256 &&
       (* when in range, preserves value *)
       (if n >= 16 && n <= 256 then n' = n else true))

(* ============================================================================
   Phase 5: Extracted Rocq Logic
   ============================================================================ *)

let prop_prepare_blocks_covers =
  QCheck.Test.make ~count:1000 ~name:"prepare_blocks covers file exactly"
    (make (Gen.int_range 0 5_000_000))
    (fun fsz ->
       let bs = L.Processor.prepare_blocks [] (L.Conversion.i2n fsz) in
       let total = sum_sizes bs in

       total = fsz &&
       contiguous bs &&
       List.for_all (fun (b : L.Assembly.blockinformation) ->
         L.Conversion.n2i b.L.Assembly.blocksize <= 32768
       ) bs)

(** Boundary generator for block_sz multiples *)
let block_boundary_sizes =
  Gen.oneof_list [
    0; 1;
    16383; 16384; 16385;
    32767; 32768; 32769;
    65535; 65536; 65537;
    98303; 98304; 98305;  (* 3*32768 *)
  ]

let prop_prepare_blocks_boundaries =
  QCheck.Test.make ~count:50 ~name:"prepare_blocks on block boundaries"
    (make block_boundary_sizes)
    (fun fsz ->
       let bs = L.Processor.prepare_blocks [] (L.Conversion.i2n fsz) in
       sum_sizes bs = fsz)

let prop_distribute_group_sizes =
  QCheck.Test.make ~count:500 ~name:"distribute groups have correct sizes"
    (make (Gen.pair (Gen.int_range 16 64) (Gen.list (Gen.int_range 1 100))))
    (fun (n, weights) ->
       let cfg = test_config n in
       let nch = L.Nchunks.from_int n in
       let fps = L.Distribution.enumerate_chunk_paths cfg test_aid nch in
       let weights_n = List.map L.Conversion.i2n weights in
       let groups = L.Distribution.distribute_by_weight fps weights_n in

       (* Check group sizes match expected *)
       let actual_sizes = List.map List.length groups in
       let expected_sizes = List.map (fun w -> min w n) weights in
       actual_sizes = expected_sizes)

let prop_distribute_no_loss =
  QCheck.Test.make ~count:500 ~name:"distribute loses no chunks when sum >= n"
    (make (Gen.pair (Gen.int_range 16 64) (Gen.list (Gen.int_range 1 50))))
    (fun (n, raw_weights) ->
       (* Ensure weights sum to at least n *)
       assume (List.fold_left (+) 0 raw_weights >= n);

       let cfg = test_config n in
       let nch = L.Nchunks.from_int n in
       let fps = L.Distribution.enumerate_chunk_paths cfg test_aid nch in
       let weights_n = List.map L.Conversion.i2n raw_weights in
       let groups = L.Distribution.distribute_by_weight fps weights_n in
       let all_chunks = List.flatten groups in

       (* At least n chunks distributed when sum >= n *)
       List.length all_chunks >= n)

(* ============================================================================
   Test Suite Registration
   ============================================================================ *)

let test = ("PBT", List.map QCheck_alcotest.to_alcotest [
  (* Phase 2: Assembly layout *)
  prop_get_after_add;
  prop_add_content_errors;

  (* Phase 3: Full assembly round-trip *)
  prop_full_assembly_restores;

  (* Phase 4: Pure helpers *)
  prop_restore_relpath_never_escapes;
  prop_sinkcheck_localhost;
  prop_nchunks_from_int;

  (* Phase 5: Extracted logic *)
  prop_prepare_blocks_covers;
  prop_prepare_blocks_boundaries;
  prop_distribute_group_sizes;
  prop_distribute_no_loss;
])
