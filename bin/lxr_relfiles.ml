(* [@@@warning "-32"] *)

open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils

open Mlcpp_chrono

let def_myid = "1234567890"

let arg_verbose = ref false
let arg_bm = ref false
let arg_irmin = ref false
let arg_files = ref []
let arg_dbpath = ref (Filename.concat (Filename.get_temp_dir_name ()) "db")
let arg_myid = ref def_myid

let usage_msg = "lxr_relfiles [-v] [-i myid] [-n nchunks] [-d dbpath] <file1> [<file2>] ..."

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-b", Arg.Set arg_bm, "benchmark");
    ("-x", Arg.Set arg_irmin, "test irmin");
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

(* let example_output () =
  let config : Configuration.configuration =
    { config_nchunks = Nchunks.from_int 16
    ; path_chunks = "lxr"
    ; path_db = Filename.concat (Filename.get_temp_dir_name ()) "db"
    ; my_id = "4242"
    ; trace = Tracer.nullTracer } in
  let%lwt rel = Relfiles.new_map config in
  let blocks1 = [({ blockid = Conversion.i2p 1; blockaid = "aid001";
                    filepos = Conversion.i2n 0; blocksize = Conversion.i2n 1200;
                    bchecksum = "check1"; blockapos = Conversion.i2n 37001
                  } : Assembly.blockinformation)
                ; { blockid = Conversion.i2p 2; blockaid = "aid001";
                    filepos = Conversion.i2n 1200; blocksize = Conversion.i2n 670;
                    bchecksum = "check2"; blockapos = Conversion.i2n (37001+1200)
                  } ] in
  let blocks2 = [({ blockid = Conversion.i2p 1; blockaid = "aid001";
                    filepos = Conversion.i2n 0; blocksize = Conversion.i2n 1200;
                    bchecksum = "check1"; blockapos = Conversion.i2n (37001+1200+670)
                  } : Assembly.blockinformation)
                ; { blockid = Conversion.i2p 2; blockaid = "aid001";
                    filepos = Conversion.i2n 1200; blocksize = Conversion.i2n 860;
                    bchecksum = "check2"; blockapos = Conversion.i2n (37001+1200+670+1200)
                  }
                ; { blockid = Conversion.i2p 3; blockaid = "aid002";
                    filepos = Conversion.i2n (1200+860); blocksize = Conversion.i2n 323;
                    bchecksum = "check3"; blockapos = Conversion.i2n 0
                  } ] in
  let fhash1 = Elykseer_crypto.Sha3_256.string ("testfile01.data" ^ config.my_id) in
  let fhash2 = Elykseer_crypto.Sha3_256.string ("testfile02.data" ^ config.my_id) in
  let rel1 : Relfiles.relation = { rfi={fname="testfile01.data";fhash=fhash1;fsize=Conversion.i2n 173;fowner="";fpermissions=Conversion.i2n 644;fmodified="";fchecksum=""}
             ; rfbs=blocks1 } in
  let rel2 : Relfiles.relation = { rfi={fname="testfile02.data";fhash=fhash2;fsize=Conversion.i2n 324;fowner="";fpermissions=Conversion.i2n 644;fmodified="";fchecksum=""}
             ; rfbs=blocks2 } in
  let%lwt _ = Relfiles.add fhash1 rel1 rel in
  let%lwt _ = Relfiles.add fhash2 rel2 rel in
  let%lwt _ = Relfiles.close_map rel in
  Lwt_io.printl "done." *)


module Config = struct
  let init () = ()
  let root = Filename.concat (Filename.get_temp_dir_name ()) "db"
end

open Lwt.Syntax

let info = Irmin_git_unix.info

module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)

let update t k v =
  let msg = Fmt.str "Updating /%s" (String.concat "/" k) in
  print_endline msg;
  Store.set_exn t ~info:(info "%s" msg) k v

let read_exn t k =
  let msg = Fmt.str "Reading /%s" (String.concat "/" k) in
  print_endline msg;
  Store.get t k

let test_irmin () =
  Config.init ();
  let config = Irmin_git.config ~bare:true Config.root in
  let* repo = Store.Repo.v config in
  let* t = Store.main repo in
  let* () = update t [ "root"; "misc"; "1.txt" ] "Hello world!" in
  let* () = update t [ "root"; "misc"; "2.txt" ] "Hi!" in
  let* () = update t [ "root"; "misc"; "3.txt" ] "How are you ?" in
  let* _ = read_exn t [ "root"; "misc"; "2.txt" ] in
  let* x = Store.clone ~src:t ~dst:"test" in
  print_endline "cloning ...";
  let* () = update t [ "root"; "misc"; "3.txt" ] "Hohoho" in
  let* () = update x [ "root"; "misc"; "2.txt" ] "Cool!" in
  let* r = Store.merge_into ~info:(info "t: Merge with 'x'") x ~into:t in
  match r with
  | Error _ -> failwith "conflict!"
  | Ok () ->
      print_endline "merging ...";
      let* _ = read_exn t [ "root"; "misc"; "2.txt" ] in
      let+ _ = read_exn t [ "root"; "misc"; "3.txt" ] in
      ()

let output_file_blocks fn relfiles : unit Lwt.t =
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

let output_blocks fns relfiles =
  let%lwt () = Lwt_io.printl "\"version\",\"fhash\",\"blockid\",\"blocksize\",\"filepos\",\"aid\",\"apos\",\"bchecksum\"" in
  Lwt_list.iter_s (fun (fn : string) -> output_file_blocks fn relfiles) fns

(* main *)
let main () = Arg.parse argspec anon_args_fun usage_msg;
  if !arg_bm
    then
      let%lwt () = benchmark_run 1_000 in
      (* let%lwt () = benchmark_run 100_000 in *)
      Lwt.return ()
    else if !arg_irmin
        then
          test_irmin ()
        else if !arg_files != []
          then 
            let nchunks = Nchunks.from_int 16 in  (* not needed *)
            let myid = !arg_myid in
            let conf : configuration = {
                            config_nchunks = nchunks;
                            path_chunks = "lxr";
                            path_db     = !arg_dbpath;
                            my_id       = myid;
                            trace       = Tracer.nullTracer } in
            let%lwt relfiles = Relfiles.new_map conf in
            output_blocks !arg_files relfiles
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
