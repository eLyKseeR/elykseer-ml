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

let def_myid = "1234567890"

let arg_verbose = ref false
let arg_files = ref []
let arg_dbpath = ref "/tmp/db"
let arg_myid = ref def_myid

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files


let verify_file fname (fblocks : Assembly.blockinformation list) =
  Cstdio.File.fopen fname "rx" |> function
    | Ok fptr ->
      let verify_block fptr (fb : Assembly.blockinformation) =
          let fpos = Conversion.n2i fb.filepos in
          Cstdio.File.fseek fptr fpos |> function
            | Ok _ -> begin
              let sz = Conversion.n2i fb.blocksize in
              let buf = Cstdio.File.Buffer.create sz in
              Cstdio.File.fread buf sz fptr |> function
                | Ok _nread ->
                  let chk = fb.bchecksum = Elykseer_base.Buffer.sha256 buf in
                  if !arg_verbose then
                    let (res,res') = if chk then ("+","✅") else ("-","❌") in
                    let () = Lwt_io.printlf " %s%s block %d@%d=%d" res res' (Conversion.p2i fb.blockid) (Conversion.n2i fb.filepos) (Conversion.n2i fb.blocksize) |> ignore in chk
                  else chk
                | Error (errno,errstr) -> 
                  Printf.printf " -❌ block %d@%d=%d read error no:%d err:%s \n" (Conversion.p2i fb.blockid) (Conversion.n2i fb.filepos) (Conversion.n2i fb.blocksize) errno errstr; false
              end
            | Error (errno,errstr) -> 
                Printf.printf "seek error no:%d err:%s\n" errno errstr; false
      in
      let res = List.fold_left (fun acc fb -> match verify_block fptr fb with true -> acc | false -> false) true fblocks in
      Cstdio.File.fclose fptr |> ignore;
      res
    | Error (errno,errstr) -> 
        Printf.printf "open error no:%d err:%s\n" errno errstr |> ignore; false

let rec additional_fblocks' (maxfb : Assembly.blockinformation) fsz acc =
  let bsz = N.sub (N.sub fsz maxfb.filepos) maxfb.blocksize in
  let () = if !arg_verbose then
      Format.printf "fsz = %d  maxfb = %d=%d => bsz = %d\n" (Conversion.n2i fsz) (Conversion.n2i maxfb.filepos) (Conversion.n2i maxfb.blocksize) (Conversion.n2i bsz)
    else () in
  if N.compare bsz N0 = Gt then
    let bsz' = if N.compare bsz BackupPlanner.max_block_size = Gt then BackupPlanner.max_block_size else bsz in
    let maxfb' : Assembly.blockinformation =
      { blockid = Conversion.i2p (1 + Conversion.p2i maxfb.blockid); bchecksum = ""; blocksize = bsz';
        filepos = N.add maxfb.filepos maxfb.blocksize; blockaid = ""; blockapos = Conversion.i2n 0 } in
    additional_fblocks' maxfb' fsz (maxfb' :: acc)
  else acc
let additional_fblocks (maxfb : Assembly.blockinformation) fsz =
  additional_fblocks' maxfb fsz []
let make_fblocks (r : Relfiles.relation) =
  let fstb = List.hd r.rfbs in
  let maxfb = List.fold_left (fun (maxfb : Assembly.blockinformation) (fb : Assembly.blockinformation) ->
                                        if N.compare fb.filepos maxfb.filepos = Gt
                                          then fb
                                          else maxfb)
                                       fstb r.rfbs in
  (* let fsz0 = r.rfi.fsize in *)
  let fsz = Elykseer_base.Fsutils.fsize r.rfi.fname |> Conversion.i2n in
  let addbs = additional_fblocks maxfb fsz in
  List.append addbs r.rfbs |> List.rev
let main () = Arg.parse argspec anon_args_fun "lxr_compare: vdi";
  let nchunks = Nchunks.from_int 16 in
  if List.length !arg_files > 0
  then
    let myid = !arg_myid in
    let conf : configuration = {
                  config_nchunks = nchunks;
                  path_chunks = "lxr";
                  path_db     = !arg_dbpath;
                  my_id       = myid } in
    let%lwt relfiles = Relfiles.new_map conf in
    let%lwt res = Lwt_list.map_s (fun fn ->
          let%lwt () = if !arg_verbose then Lwt_io.printlf "comparing file %s against meta data" fn else Lwt.return_unit in
          let fhash = sha256 fn in
          let%lwt ref = Relfiles.find fhash relfiles in
          match ref with
          | None -> Lwt.return false
          | Some r ->
            let fbs = make_fblocks r in
            Lwt.return (verify_file fn fbs)
        ) !arg_files in
    let cnt_all = List.length !arg_files in
    let cnt_success = List.fold_left (fun acc e -> if e then acc + 1 else acc) 0 res in
    if !arg_verbose then
      Lwt_io.printlf "comparison of %d %s with %d equal" cnt_all (Utils.pluralise "file" cnt_all) cnt_success
      (* return with an exit code of 0 when all files match, otherwise the number of differing files *)
    else Stdlib.exit (cnt_all - cnt_success)
  else
    Lwt.return ()

let () = Lwt_main.run (main ())
