(* [@@@warning "-32"] *)

(*  call in shell to analyse the program's exit code:
     lxr_compare.exe  -i 424242 -d ../elykseer.db test1M && echo YES || echo NO

    call with verbose flag set to get block by block information:
     lxr_compare.exe -v -i 424242 -d ../elykseer.db test1M
*)
open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_base.Hashing
open Elykseer_utils

open Mlcpp_cstdio

let def_myid = 1234567890

let arg_verbose = ref false
let arg_dryrun = ref false
let arg_files = ref []
let arg_dbpath = ref "/tmp/db"
let arg_chunkpath = ref "lxr"
let arg_myid = ref def_myid
let arg_nchunks = ref 16

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-y", Arg.Set arg_dryrun, "dry run");
    ("-x", Arg.Set_string arg_chunkpath, "sets output path for encrypted chunks");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
    ("-i", Arg.Set_int arg_myid, "sets own identifier (positive number)");
    ("-n", Arg.Set_int arg_nchunks, "sets number of chunks (16-256) per assembly");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files


let verify_file acontroller fname (fblocks : Assembly.blockinformation list) =
  Cstdio.File.fopen fname "rx" |> function
    | Ok fptr ->
      let verify_block actrl fptr (fb : Assembly.blockinformation) : (Actrl.t * Assembly.blockinformation) Lwt.t =
          let fpos = Conversion.n2i fb.filepos in
          Cstdio.File.fseek fptr fpos |> function
            | Ok _ -> begin
              let sz = Conversion.n2i fb.blocksize in
              let buf = Cstdio.File.Buffer.create sz in
              Cstdio.File.fread buf sz fptr |> function
                | Ok nread -> begin
                  if nread = sz then
                    let newchksum = Elykseer_base.Buffer.sha256 buf in
                    let chk = fb.bchecksum = newchksum in
                    let prtverbose = if !arg_verbose then
                        let (res,res') = if chk then ("+","✅") else ("-","❌") in
                        Lwt_io.printlf " %s%s block %d@%d=%d" res res' (Conversion.p2i fb.blockid) (Conversion.n2i fb.filepos) (Conversion.n2i fb.blocksize) |> ignore
                      else () in
                    if !arg_dryrun then
                      let () = prtverbose in Lwt.return (actrl,fb)
                    else
                      let () = prtverbose in
                      if chk then Lwt.return (actrl,fb)
                      else
                        Actrl.addblock actrl fname {fb with bchecksum = newchksum} buf
                  else
                    let () = Printf.printf "read returned wrong size: %d; should be %d\n" nread sz in Lwt.return (actrl,fb)
                  end
                | Error (errno,errstr) -> 
                  let () = Printf.printf "read error no:%d err:%s\n" errno errstr in Lwt.return (actrl,fb)
              end
            | Error (errno,errstr) -> 
                let () = Printf.printf "seek error no:%d err:%s\n" errno errstr in Lwt.return (actrl,fb)
      in
      let%lwt (ac', fblocks') = Lwt_list.fold_left_s (fun (ac,fbs) fb -> let%lwt (ac', fb') = verify_block ac fptr fb in Lwt.return (ac', fb' :: fbs)) (acontroller, []) fblocks in
      let () = Cstdio.File.fclose fptr |> ignore in Lwt.return (ac', fblocks')
    | Error (errno,errstr) -> 
        let () = Printf.printf "open error no:%d err:%s\n" errno errstr in Lwt.return (acontroller, fblocks)

let rec additional_fblocks' (maxfb : Assembly.blockinformation) fsz acc =
  let bsz = N.sub (N.sub fsz maxfb.filepos) maxfb.blocksize in
  (* let () = if !arg_verbose then
      Format.printf "fsz = %d  maxfb = %d=%d => bsz = %d\n" (Conversion.n2i fsz) (Conversion.n2i maxfb.filepos) (Conversion.n2i maxfb.blocksize) (Conversion.n2i bsz)
    else () in *)
  if N.compare bsz N0 = Gt then
    let bsz' = if N.compare bsz BackupPlanner.max_block_size = Gt then BackupPlanner.max_block_size else bsz in
    let maxfb' : Assembly.blockinformation =
      { blockid = Conversion.i2p (1 + Conversion.p2i maxfb.blockid); bchecksum = ""; blocksize = bsz';
        filepos = N.add maxfb.filepos maxfb.blocksize; blockaid = ""; blockapos = Conversion.i2n 0 } in
    additional_fblocks' maxfb' fsz (maxfb' :: acc)
  else acc

let additional_fblocks (maxfb : Assembly.blockinformation) fsz =
  additional_fblocks' maxfb fsz []

let make_fblocks (fi : Filesupport.fileinformation) (r : Relfiles.relation) =
  let fsz = fi.fsize in
  let cleanfbs = List.filter (fun (fb : Assembly.blockinformation) -> N.ltb fb.filepos fsz) r.rfbs in
  let fstb = List.hd cleanfbs in
  let maxfb = List.fold_left (fun (maxfb : Assembly.blockinformation) (fb : Assembly.blockinformation) ->
                                        if N.ltb fb.filepos fsz && N.ltb maxfb.filepos fb.filepos
                                          then fb
                                          else maxfb)
                             fstb cleanfbs in
  let addbs = additional_fblocks maxfb fsz in
  List.append addbs cleanfbs |> List.rev

let main () = Arg.parse argspec anon_args_fun "lxr_incremental: vyxdin";
  let nchunks = Nchunks.from_int !arg_nchunks in
  if List.length !arg_files > 0
    then
      let myid = let id0 = !arg_myid in
                if id0 >= 0 then id0 else def_myid in
      let conf : configuration = {
                    config_nchunks = nchunks;
                    path_chunks = !arg_chunkpath;
                    path_db     = !arg_dbpath;
                    my_id       = Conversion.i2n myid } in
      let%lwt relfiles = Relfiles.new_map conf in
      let%lwt acontroller = Actrl.create conf in
      let%lwt (res, actrl') = Lwt_list.fold_left_s (fun (res,actrl) fn ->
            let%lwt () = if !arg_verbose then Lwt_io.printlf "comparing file %s against meta data" fn else Lwt.return_unit in
            let fhash = sha256 fn in
            let%lwt ref = Relfiles.find fhash relfiles in
            match ref with
            | None -> 
              let%lwt () = Lwt_io.printlf "  no meta data found for file=%s (%s)!" fhash fn in
              Lwt.return (true :: res, actrl)
            | Some r ->
              let fi = Filesupport.get_file_information fn in
              if fi.fchecksum = r.rfi.fchecksum then
                let%lwt () = Lwt_io.printlf "  checksum is equal for file=%s (%s)!" fhash fn in
                Lwt.return (true :: res, actrl)
              else (* checksums are not equal *)
                let fbs = make_fblocks fi r in
                let%lwt (actrl', fbs') = verify_file actrl fn fbs in
                let%lwt _rel' = if !arg_dryrun then Lwt.return relfiles
                         else Relfiles.add fhash {rfi=fi; rfbs=fbs'} relfiles in
                let%lwt () = Lwt_io.printlf "  blocks changed for file=%s (%s)!" fhash fn in
                Lwt.return (false :: res, actrl')
          ) ([], acontroller) !arg_files in
      let%lwt () = Relfiles.close_map relfiles in
      let%lwt () = Actrl.stop actrl' in
      let cnt_all = List.length !arg_files in
      let cnt_success = List.fold_left (fun acc e -> if e then acc + 1 else acc) 0 res in
      if !arg_verbose then
        Lwt_io.printlf "comparison of %d %s with %d equal" cnt_all (Utils.pluralise "file" cnt_all) cnt_success
        (* return with an exit code of 0 when all files match, otherwise the number of differing files *)
      else Stdlib.exit (cnt_all - cnt_success)
    else
      Lwt.return ()

let () = Lwt_main.run (main ())
