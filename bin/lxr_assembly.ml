(* open Lwt *)

open Elykseer__Lxr
open Elykseer__Lxr.Buffer
open Elykseer__Lxr.Assembly
open Elykseer__Lxr.Configuration
(* open Elykseer__Lxr.Conversion *)
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

let bm_add_content content buffer sz =
    Assembly.add_content ~src:content ~sz:sz ~pos:0 ~tgt:buffer

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
    let%lwt () = Lwt_io.printlf "rnd256 %d" (Conversion.n2i @@ Utilities.rnd @@ Conversion.i2n 0) in
    let%lwt () = Lwt_io.printlf "rnd256 %d" (Conversion.n2i @@ Utilities.rnd @@ Conversion.i2n 0) in
    let%lwt () = Lwt_io.printlf "sha256 of /bin/sh %s" (Fsutils.fchksum "/bin/sh") in
    let c : Configuration.configuration = { config_nchunks = (Nchunks.from_int 16); path_chunks = "./chunks"; path_db = "/tmp/db"; my_id = "16"} in
    let e0 = Environment.EnvironmentWritable.initial_environment c in
    let a = Environment.cur_assembly e0 in
    let b = Environment.cur_buffer e0 in
    let%lwt () = Lwt_io.printlf "assembly %s %d %d" a.aid (Conversion.p2i a.nchunks) (Conversion.n2i a.apos) in
    let%lwt () = Lwt_io.printlf "block size %d" (Conversion.n2i @@ AssemblyPlainWritable.buffer_len b) in
    let%lwt () = Lwt_io.printl (Utils.as2s a) in
    let%lwt () = Lwt_io.printlf "buffer sha256 = %s" (AssemblyPlainWritable.calc_checksum b) in
    (* let msg = "testing some longer message." in *)
    let content = BufferPlain.buffer_create (Conversion.i2n 1024) in (*  (fun i -> if i < 28 then String.get msg i else '0') in *)
    let (a', bi) = Elykseer__Lxr.Assembly.backup a b (* "test1M" *) (Conversion.i2n 0) content in
    let e1 = Environment.EnvironmentWritable.env_add_file_block "test1M" e0 bi in
    let e2 = Environment.EnvironmentWritable.env_add_aid_key (aid a') e1 {pkey="abc97391af";ivec="323453";localnchunks=Nchunks.to_positive c.config_nchunks;localid=c.my_id} in
    let relkeys = Environment.keys e2 in
    let relkey = List.assoc (aid a') relkeys in
    let (a'', b') = Elykseer__Lxr.Assembly.finish a' b in
    match Elykseer__Lxr.Assembly.encrypt a'' b' relkey with
    | None -> Lwt_io.printl "failed to encrypt"
    | Some (_a''', b'') -> Lwt_io.printlf "sha256 = %s" (AssemblyEncrypted.calc_checksum b'') |> ignore ;
    benchmark_add_content

let () = Lwt_main.run (main ())
