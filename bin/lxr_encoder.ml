(* open Lwt *)

open Elykseer__Lxr
open Elykseer__Lxr.Assembly
open Elykseer__Lxr.Block
open Elykseer__Lxr.Environment

open Elykseer_utils

let arg_verbose = ref false
let arg_fctrl = ref ""

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-f", Arg.Set_string arg_fctrl, "control file for LXR encoder");
  ]

let anon_args_fun _fn = ()

(* let mklist n dec =
  let rec loop k l =
    if k <= 0 then l else loop (k - dec) (k :: l) in
  loop n [] *)

(* the block size *)
(* let blocksize = ref 2048;; *)


(* let read_file_block _ch _buf _apos bsz =
    if bsz <= 0 then Lwt.return 0
    else
      (* Lwt_io.read_into_bigstring ch !buf apos 2024 *)
      (* Lwt_io.read_into ch buf apos 2024 *)
      Lwt.return (min bsz 2024) *)

(* let read_file_blocks ch buf apos0 bsz0 bn0 fpos0 banum =
    let apos = ref apos0 in
    let bsz = ref bsz0 in
    let bn = ref bn0 in
    let fpos = ref fpos0 in
    Lwt_stream.fold_s (fun i agg ->
        let%lwt () = Lwt_io.printf "@%d: reading from %d len=%d into pos=%d\n" i !fpos !bsz !apos in
        match%lwt read_file_block ch buf !apos !bsz with
        | 0 -> Lwt.return agg
        | n -> let%lwt () = Lwt_io.printf "read %d\n\n" n in
               let bs = { blockid = Conversion.i2p !bn
                        ; blocksize = Conversion.i2n n
                        ; filepos = Conversion.i2n !fpos
                        ; blockanum = banum
                        ; blockapos = Conversion.i2n !apos } :: agg in
               apos := !apos + n;
               bsz := !bsz - n;
               bn := !bn + 1;
               fpos := !fpos + n;
               Lwt.return bs )
        (Lwt_stream.of_list @@ mklist !bsz !blocksize)
        [] *)

(* let read_file_blocks _ch _buf apos _bsz0 bn fpos banum =
    let bi = { blockid = Conversion.i2p bn
             ; bchecksum = ""
             ; blocksize = Conversion.i2n 42
             ; filepos = Conversion.i2n fpos
             ; blockanum = banum
             ; blockapos = Conversion.i2n apos } in
    Lwt.return [bi] *)

let encode_file fb _buf : fileblocks Lwt.t =
    let%lwt () = Lwt_io.printl (Utils.fibs2s fb) in
    let%lwt _bs = Lwt_io.with_file ~mode:Input fb.bfi.fname
      (fun _ch ->
        Lwt_stream.fold_s (fun b _agg ->
            (* fseek ch b.filepos *)
            let _fpos = (Conversion.n2i b.filepos) in
            let _bsz = (Conversion.n2i b.blocksize) in
            (* let%lwt () = Lwt_io.set_position ch @@ Int64.of_int fpos in *)
            (* let fbytes = fread ch b.blocksize in *)
            (* let%lwt bs = read_file_blocks ch buf (Conversion.n2i b.blockapos) bsz
                                                  0 fpos b.blockanum  in *)
            (* let n = List.fold_left (fun agg b -> agg + Conversion.n2i b.blocksize) 0 bs in *)
            (* let%lwt () = Lwt_io.printf "read %d bytes of %d\n" n bsz in *)
            (* add_to_buf buf b.blockapos fbytes *)
            (* Lwt.return (bs @ agg) ) *)
            Lwt.return [] )
            (Lwt_stream.of_list fb.blocks)
            []
      ) in
    Lwt.return fb 
    (* { bfi = fb.bfi; blocks = bs } *)

let encode e : fileblocks list Lwt.t =
    let len =   (Conversion.n2i chunkwidth_N)
              * (Conversion.n2i chunklength_N)
              * (Conversion.p2i (nchunks (List.hd e.assemblies))) in
    let%lwt () = Lwt_io.printf "assembly size %d bytes\n" len in
    (* let buf = ref (Lwt_bytes.create len) in *)
    (* let buf = Bytes.create len in *)
    let buf = [0] in
    Lwt_stream.fold_s (fun fb agg -> let%lwt fb' = encode_file fb buf in
                                     Lwt.return (fb' :: agg))
                      (Lwt_stream.of_list e.files)
                      []

let encode_check e : environment Lwt.t =
    if List.length e.assemblies = 1
    then let%lwt fs = encode e in
         let%lwt () = Lwt_io.printf "encoded %d files" (List.length fs) in
         (* Lwt.return e *)
         let cnt = List.fold_left (fun agg f -> agg + List.fold_left (fun agg b -> agg + Conversion.n2i b.blocksize) 0 f.blocks) 0 e.files in
         Lwt.return { cur_assembly = e.cur_assembly
                    ; count_input_bytes = Conversion.i2n cnt
                    ; config = e.config
                    ; files = fs
                    ; assemblies = e.assemblies }
    else Lwt.return e

(* main *)
let main () = Arg.parse argspec anon_args_fun "lxr_encoder: vf";
    let _setup = setup_environment in
    let e = Envutils.envrestore !arg_fctrl in
    let%lwt () = Lwt_io.printl (Utils.e2s e) in
    let%lwt e' = encode_check e in
    Lwt_io.printf "encoded %d bytes\n" @@ Conversion.n2i(e'.count_input_bytes)

let () = Lwt_main.run (main ())
