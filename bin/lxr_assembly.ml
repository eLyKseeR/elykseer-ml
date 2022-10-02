(* open Lwt *)

open Elykseer__Lxr
open Elykseer__Lxr.Buffer
open Elykseer__Lxr.Assembly
open Elykseer__Lxr.Configuration
(* open Elykseer__Lxr.Conversion *)
(* open Elykseer__Lxr.Block *)
(* open Elykseer__Lxr.Environment *)

open Elykseer_utils
open Elykseer_base

open Mlcpp_cstdio
open Mlcpp_chrono


let arg_verbose = ref false
let arg_fctrl = ref ""
let arg_aid = ref ""

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-a", Arg.Set_string arg_aid, "id of assembly");
    ("-f", Arg.Set_string arg_fctrl, "control file for LXR encoder");
  ]

let anon_args_fun _fn = ()

(* let assembly_check e p_aid : assembly option Lwt.t =
    Lwt.return @@ List.find_opt (fun a -> aid a = p_aid) e.assemblies *)

let bm_add_content content buffer sz =
    (* let buffer = Cstdio.File.Buffer.create (16*256*1024) in *)
    Assembly.add_content content sz 0 buffer

let benchmark_add_content =
  let msg = "testing some longer message." in
  let content = Cstdio.File.Buffer.init (16*256*1024) (fun i -> if i < 28 then String.get msg i else '0') in
  let clock1 = Chrono.Clock.System.now () in
  let buffer = Cstdio.File.Buffer.create (16*256*1024) in
  for _i = 0 to 999 do
    let bench = bm_add_content content buffer (1*256*1024) in
    if bench = (1*256*1024) then print_string "√" else print_char '?'
  done;
  let clock2 = Chrono.Clock.System.now () in
  let tdiff = Chrono.Clock.System.diff clock2 clock1 in
  Lwt_io.printlf "time: %s" (Chrono.Duration.to_string @@ Chrono.Duration.cast_ms tdiff)

(* main *)
let main () = Arg.parse argspec anon_args_fun "lxr_assembly: vaf";
    (* let _setup = setup_environment in
    let e = Envutils.envrestore !arg_fctrl in
    let%lwt () = Lwt_io.printl (Utils.e2s e) in
    let%lwt a = assembly_check e !arg_aid in *)
    let c : Configuration.configuration = { path_chunks = "./chunks"; path_meta = "./meta"; my_id = Conversion.i2n 16} in
    let (a,b) = AssemblyPlainWritable.create c (Nchunks.from_int 16) in
    let%lwt () = Lwt_io.printf "assembly %s %d %d" a.aid (Conversion.p2i a.nchunks) (Conversion.n2i a.apos) in
    let%lwt () = Lwt_io.printf "block size %d" (Conversion.n2i @@ AssemblyPlainWritable.buffer_len b) in
    let%lwt () = Lwt_io.printl (Utils.as2s a) in
    let%lwt () = Lwt_io.printlf "buffer sha256 = %s" (AssemblyPlainWritable.calc_checksum b) in
    (* let msg = "testing some longer message." in *)
    let content = BufferPlain.buffer_create (Conversion.i2n 1024) in (*  (fun i -> if i < 28 then String.get msg i else '0') in *)
    let relfiles = RelationFileAid.coq_new in
    let (a', _relfiles') = backup a b "test1M" content relfiles in
    let relkey = (RelationAidKey.add "abc97391af" (aid a') RelationAidKey.coq_new ) in
    let (a'', b') = Elykseer__Lxr.Assembly.finish a' b in
    match Elykseer__Lxr.Assembly.encrypt a'' b' relkey with
    | None -> Lwt_io.printl "failed to encrypt"
    | Some (_a''', b'') -> Lwt_io.printlf "sha256 = %s" (AssemblyEncrypted.calc_checksum b'') |> ignore ;
    benchmark_add_content

let () = Lwt_main.run (main ())
