(* open Lwt *)

open Elykseer__Lxr
open Elykseer__Lxr.Assembly
open Elykseer__Lxr.Block
open Elykseer__Lxr.Environment

open Elykseer_utils

open Mlcpp_filesystem
open Mlcpp_cstdio

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

let add_data assembly buf apos =
    let len = Cstdio.File.Buffer.size buf in
    let nchunks = assembly.nchunks in
    let rbufin = ref buf in
    let rchunks = ref assembly.chunks in
    let rec add_data' rchunks bpos len rbufin apos0 =
        if bpos < len
        then
          let (chnum, chidx) = apos_to_chidx nchunks (Conversion.i2n(bpos + apos0)) in
          let chunk = List.nth !rchunks (Conversion.n2i chnum) in
          let c = Cstdio.File.Buffer.get !rbufin bpos in
          Cstdio.File.Buffer.set chunk.buffer.buf (Conversion.n2i chidx) c;
          add_data' rchunks (succ bpos) len rbufin apos0
        else assembly in
    add_data' rchunks 0 len rbufin apos

let update_assembly e assembly =
    { e with assemblies = [assembly] }

let rec encode_blocks fptr e assembly bs =
    match bs with
    | [] -> e
    | b :: rbs -> let fpos = (Conversion.n2i b.filepos) in
                  let apos = (Conversion.n2i b.blockapos) in
                  let bsz = (Conversion.n2i b.blocksize) in
                  Cstdio.File.fseek fptr fpos |> function
                  | Ok () -> begin
                       let buf = Cstdio.File.Buffer.create bsz in
                       Cstdio.File.fread buf bsz fptr |> function
                       | Ok nread -> begin
                            Printf.eprintf "encode_blocks read %d\n" nread;
                            let a' = add_data assembly buf apos in
                            let e' = update_assembly e a' in
                            encode_blocks fptr e' a' rbs
                        end
                       | Error (errno,errstr) -> Printf.eprintf "encode_blocks 2 error %d: %s\n" errno errstr; e
                      end
                  | Error (errno,errstr) -> Printf.eprintf "encode_blocks 1 error %d: %s\n" errno errstr; e

let rec encode_fileblocks e assembly fb : environment =
    match fb with
    | [] -> e
    | f :: rfs -> let fp = Filesystem.Path.from_string (f.bfi.fname) in
        if Filesystem.Path.exists fp
        then
          Cstdio.File.fopen (Filesystem.Path.to_string fp) "rx" |> function
          | Ok fptr -> let e' = encode_blocks fptr e assembly (f.blocks) in
                       Cstdio.File.fclose fptr |> ignore;
                       encode_fileblocks e' assembly rfs
          | Error (errno,errstr) -> Printf.eprintf "encode_fileblocks error %d: %s\n" errno errstr; e
        else
          e

(*

performance: 4.41s for 16 chunk-wide assembly
*)
let encode e : environment Lwt.t =
    let assembly = List.hd e.assemblies in
    Lwt.return @@ encode_fileblocks e assembly e.files

let encode_check e : environment Lwt.t =
    if List.length e.assemblies = 1
    then let%lwt e' = encode e in
         let%lwt () = Lwt_io.printf "encoded to %s\n" (Utils.e2s e') in
         Lwt.return e'
    else let%lwt () = Lwt_io.printf "error: expected only one assembly, got %d\n" (List.length e.assemblies) in
         Lwt.return e

(* main *)
let main () = Arg.parse argspec anon_args_fun "lxr_encoder: vf";
    let _setup = setup_environment in
    let e = Envutils.envrestore !arg_fctrl in
    let%lwt () = Lwt_io.printl (Utils.e2s e) in
    let%lwt e' = encode_check e in
    Lwt_io.printf "encoded %d bytes\n" @@ Conversion.n2i(e'.count_input_bytes)

let () = Lwt_main.run (main ())
