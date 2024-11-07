(* [@@@warning "-32"] *)

open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils

open Mlcpp_chrono

let def_myid = "1234567890"

let arg_verbose = ref false
let arg_xml = ref false
let arg_bm = ref false
let arg_files = ref []
let arg_dbpath = ref (Filename.concat (Filename.get_temp_dir_name ()) "db")
let arg_myid = ref def_myid

let usage_msg = "lxr_relfiles [-v] [-i myid] [-n nchunks] [-d dbpath] <file1> [<file2>] ..."

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-b", Arg.Set arg_bm, "benchmark");
    ("-x", Arg.Set arg_xml, "XML output");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

let rec mk_blocks i aid apos fpos =
  match i with
  | 0 -> []
  | n ->
    let blen = 3247 in
    let block : Assembly.blockinformation
              = { blockid = Conversion.i2p n; bchecksum = "chk";
                  blocksize = Conversion.i2n blen;
                  filepos = Conversion.i2n fpos;
                  blockaid = aid; blockapos = Conversion.i2n apos } in
    block :: mk_blocks (n - 1) aid (apos + blen) (fpos + blen)
let mk_rel n aid rel =
  let rnd = Elykseer_crypto.Random.with_rng (fun rng -> Elykseer_crypto.Random.r32_range rng 1 12) in
  let blocks : Assembly.blockinformation list = mk_blocks rnd aid 1200 0 in
  let fname = Printf.sprintf "test_%04d.dat" n in
  let fhash = Elykseer_crypto.Sha3_256.string fname in
  let fi : Filesupport.fileinformation = {fname = fname; fhash = fhash
           ;fsize = Conversion.i2n @@ List.fold_left (fun acc (e : Assembly.blockinformation) -> (Conversion.n2i e.blocksize) + acc) 0 blocks
           ;fowner = ""; fpermissions = Conversion.i2n 644; fmodified = ""; fchecksum = ""} in
  (* let%lwt () = Lwt_io.printlf "   %s <- %s" fhash fname in *)
  let%lwt _ = Relfiles.add fhash {rfi=fi; rfbs=blocks} rel in
  Lwt.return rel

let rec prepare_bm cnt rel =
  match cnt with
  | 0 -> Lwt.return rel
  | n -> let%lwt _rel' = mk_rel n "aid00012345789" rel in
         prepare_bm (n - 1) rel

let check_bm i rel =
  let fhash = Printf.sprintf "test_%04d.dat" i |> Elykseer_crypto.Sha3_256.string in
  let%lwt blocksopt = Relfiles.find fhash rel in
  match blocksopt with
  | None -> Lwt.return 0
  | Some rel ->
    Lwt.return 
    (List.fold_left (fun acc b -> Conversion.n2i(Assembly.blocksize b) + acc) 0 rel.rfbs)

let benchmark_run cnt =
  let%lwt () = Lwt_io.printlf "benchmarking %d repetitions" cnt in
  let config : Configuration.configuration =
    { config_nchunks = Nchunks.from_int 16
    ; path_chunks = "lxr"
    ; path_db = Filename.concat (Filename.get_temp_dir_name ()) "db"
    ; my_id = "0123"
    ; trace = Tracer.nullTracer } in
  let%lwt rel = Relfiles.new_map config in
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

let csv_output_file_blocks fn relfiles : unit Lwt.t =
  let%lwt rel = Relfiles.find_v (Elykseer_crypto.Sha3_256.string (fn ^ !arg_myid)) relfiles in
  match rel with
  | None -> Lwt.return ()
  | Some (v,r) ->
    let fhash = r.rfi.fhash in
    Lwt_list.iter_s (fun (bs: Elykseer__Lxr.Assembly.blockinformation) ->
      Lwt_io.printlf "\"%s\",\"%s\",%d,%d,%d,\"%s\",%d,\"%s\""
        v fhash
        (Conversion.p2i bs.blockid)
        (Conversion.n2i bs.blocksize)
        (Conversion.n2i bs.filepos)
        bs.blockaid
        (Conversion.n2i bs.blockapos)
        bs.bchecksum
    ) r.rfbs

let xml_output_file_blocks fn relfiles : unit Lwt.t =
  let fhash = Elykseer_crypto.Sha3_256.string (fn ^ !arg_myid) in
  let%lwt rel = Relfiles.find_v fhash relfiles in
  match rel with
  | None -> Lwt.return ()
  | Some (v,r) ->
    let%lwt () = Lwt_io.printlf "<fileinformation version=\"%s\" fhash=\"%s\">" v fhash in
    let%lwt () = Lwt_io.printlf "<name>%s</name>" r.rfi.fname in
    let%lwt () = Lwt_io.printlf "<size>%d</size>" (Conversion.n2i r.rfi.fsize) in
    let%lwt () = Lwt_io.printlf "<owner>%s</owner>" r.rfi.fowner in
    let%lwt () = Lwt_io.printlf "<permissions>%d</permissions>" (Conversion.n2i r.rfi.fpermissions) in
    let%lwt () = Lwt_io.printlf "<modified>%s</modified>" r.rfi.fmodified in
    let%lwt () = Lwt_io.printlf "<checksum>%s</checksum>" r.rfi.fchecksum in
    let%lwt () = Lwt_io.printlf "<fileblocks>" in
    let%lwt () = Lwt_list.iter_s (fun (bs: Elykseer__Lxr.Assembly.blockinformation) ->
      Lwt_io.printlf "<fileblock blockid=\"%d\" blocksize=\"%d\" filepos=\"%d\" apos=\"%d\"><aid>%s</aid><checksum>%s</checksum></fileblock>"
        (Conversion.p2i bs.blockid)
        (Conversion.n2i bs.blocksize)
        (Conversion.n2i bs.filepos)
        (Conversion.n2i bs.blockapos)
        bs.blockaid
        bs.bchecksum
      ) r.rfbs in
    let%lwt () = Lwt_io.printl "</fileblocks>" in
    Lwt_io.printl "</fileinformation>"

let csv_output_blocks fns relfiles =
  let%lwt () = Lwt_io.printl "\"version\",\"fhash\",\"blockid\",\"blocksize\",\"filepos\",\"aid\",\"apos\",\"bchecksum\"" in
  Lwt_list.iter_s (fun (fn : string) -> csv_output_file_blocks fn relfiles) fns

let xml_output_blocks fns relfiles =
  let%lwt () = Lwt_io.printl "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" in
  Lwt_list.iter_s (fun (fn : string) -> xml_output_file_blocks fn relfiles) fns

(* main *)
let main () = Arg.parse argspec anon_args_fun usage_msg;
  if !arg_bm
    then
      let%lwt () = benchmark_run 1_000 in
      (* let%lwt () = benchmark_run 100_000 in *)
      Lwt.return ()
    else if !arg_files != []
      then 
        let nchunks = Nchunks.from_int 16 in  (* not needed *)
        let myid = !arg_myid in
        let conf : configuration = {
                        config_nchunks = nchunks;
                        path_chunks = "lxr";
                        path_db     = !arg_dbpath;
                        my_id       = myid;
                        trace       = if !arg_verbose then Tracer.stdoutTracerDebug else Tracer.stdoutTracerWarning } in
        let%lwt relfiles = Relfiles.new_map conf in
        if !arg_xml then
          xml_output_blocks !arg_files relfiles
        else
          csv_output_blocks !arg_files relfiles
      else
        Lwt.return ()

let () = Lwt_main.run (main ())


(** benchmark runs *)

(** add 1000 files with random number of blocks to the db:
    preparation time:  91697 ms
    verification time: 270 ms

    => the repo was quite populated so it seems that irmin has problems
    to navigate in large "directories"
    => try to structure the tree deeper, more directories
*)

(** add 1000 files with random number of blocks to the db:
    preparation time:  3654 ms
    verification time: 242 ms

    => way faster!
*)

(** 2000 repetitions: 
    preparation time:  7021 ms
    verification time: 321 ms

    => linear!
*)

(** 4000 repetitions: 
    preparation time:  14117 ms
    verification time: 630 ms

    => linear!
*)
