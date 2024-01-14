(* [@@@warning "-32"] *)

open Elykseer__Lxr
open Elykseer__Lxr.Configuration
open Elykseer__Lxr.Environment

open Elykseer_base.Hashing
open Elykseer_utils

open Mlcpp_cstdio

let def_myid = "1234567890"

let arg_verbose = ref false
let arg_dryrun = ref false
let arg_files = ref []
let arg_dbpath = ref "/tmp/db"
let arg_chunkpath = ref "lxr"
let arg_nchunks = ref 16
let arg_nproc = ref 1
let arg_myid = ref def_myid

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-y", Arg.Set arg_dryrun, "dry run");
    ("-x", Arg.Set_string arg_chunkpath, "sets output path for encrypted chunks");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
    ("-n", Arg.Set_int arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-j", Arg.Set_int arg_nproc, "sets number of parallel processes");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

(** the minimum available size in an assembly *)
(* let aminsz = 127 *)

let curr_assembly_sz n = Conversion.n2i (Assembly.assemblysize n)

(* data structure of a backup plan *)
type backup_plan_file = {
  fhash : string;
  curfi : Filesupport.fileinformation;
  curbs : BackupPlanner.fileblock list;
  reffi : Filesupport.fileinformation option;
  refbs : (Assembly.blockinformation list) option
}
type backup_plan = {
  bp : backup_plan_file list
}

let plan_backup e0 fns =
  let a = e0.cur_assembly in
  let afree = curr_assembly_sz a.nchunks - (Conversion.n2i a.apos) in
  let%lwt relfiles = Relfiles.new_map e0.config in
  let%lwt (_anum', _afree', bpfs) = Lwt_list.fold_left_s (fun (anum,afree,acc) fn ->
        let fhash = sha256 fn in
        let ((anum',afree'),fbs) = BackupPlanner.analyse_file a.nchunks afree anum fn in
        let%lwt ref = Relfiles.find fhash relfiles in
        let oreffi = match ref with None -> None | Some r -> Some r.rfi in
        let orefbs = match ref with None -> None | Some r -> Some r.rfbs in
        Lwt.return (anum', afree', {fhash = fhash; curfi = fbs.fbifi; curbs = fbs.fbifblocks; reffi = oreffi; refbs = orefbs } :: acc)
      ) (Conversion.i2p 1, Conversion.i2n afree, []) fns in
    Lwt.return {bp = List.rev bpfs}

let execute_backup_fileblock e _anum (fname,blocks) =
  Cstdio.File.fopen fname "rx" |> function
    | Ok fptr ->
      let backup_block e0 fptr fb =
          let fpos = BackupPlanner.fbfpos fb in
          Cstdio.File.fseek fptr (Conversion.n2i fpos) |> function
            | Ok _ -> begin
              let sz = Conversion.n2i fb.fbsz in
              let buf = Cstdio.File.Buffer.create sz in
              Cstdio.File.fread buf sz fptr |> function
                | Ok _nread ->
                  let bplain = Buffer.BufferPlain.from_buffer buf in
                  Environment.backup e0 fname fpos bplain
                | Error (errno,errstr) -> 
                  Printf.printf "read error no:%d err:%s\n" errno errstr; e0
              end
            | Error (errno,errstr) -> 
                Printf.printf "seek error no:%d err:%s\n" errno errstr; e0
      in
      let e1 = List.fold_left (fun env fb -> backup_block env fptr fb) e blocks in
      Cstdio.File.fclose fptr |> ignore;
      e1
    | Error (errno,errstr) -> 
        Printf.printf "open error no:%d err:%s\n" errno errstr |> ignore; e

let extract_fileblocks anum (bp : backup_plan) =
  List.map (fun bpf -> let fname = bpf.curfi.fname in
    (fname, List.filter (fun fbs -> BackupPlanner.fbanum fbs = anum) bpf.curbs)
  ) bp.bp

let rec run_backup_for_assembly e0 pid anum acount (bp : backup_plan) =
  if anum > acount then e0
  else begin
      (* Printf.printf "executing backup of assembly %d\n" anum; *)
      let anum_p = Conversion.i2p anum in
      let fileblocks' = extract_fileblocks anum_p bp in
      let e1 = List.fold_left (fun env fb -> execute_backup_fileblock env anum fb) e0 fileblocks' in
      let e2 = Environment.finalise_and_recreate_assembly e1 in
      run_backup_for_assembly e2 pid (anum + 1) acount bp
  end

(* let start_processes n =
  let rec start_processes' acc n =
    if n = 0
      then List.rev acc
      else
        let proc = n in  (* TODO create external process *)
        start_processes' (proc :: acc) (n - 1) in
  start_processes' [] n *)

let run_distributed_backup e0 nproc acount bp =
  (* start n processes *)
  let ps = (* start_processes *) nproc in
  (* distribute work among processes *)
  run_backup_for_assembly e0 ps 1 acount bp

let validate_fileblocks fname fhash bis =
    if !arg_verbose then
      let list_sum = List.fold_left (+) 0 in
      let nb = List.length bis in
      let na = List.map (fun (e : Assembly.blockinformation) -> e.blockaid) bis |> List.sort_uniq compare |> List.length in
      let sumbsz = List.map (fun (e : Assembly.blockinformation) -> Conversion.n2i e.blocksize) bis |> list_sum in
      let lbsz = List.map (fun (e : Assembly.blockinformation) -> (Conversion.n2i e.filepos, Conversion.n2i e.blocksize)) bis |>
                 List.sort (fun e1 e2 -> compare (fst e1) (fst e2)) in
      (* let%lwt () = Lwt_list.iteri_s (fun i (a,b) -> Lwt_io.printlf "    %d: %d + %d" i a b) lbsz in *)
      let sumdelta = Zip.zip (lbsz) (List.tl lbsz) |> List.map (fun ((a,b),(a',_b')) -> (a' - a) - b) |> list_sum in
      let (res,res') = if 0 = sumdelta then ("+","✅") else ("-","❌") in
      Lwt_io.printlf "%s%s '%s' (%s) in %d assemblies with %d blocks, %d bytes in total (control=%d)" res res' fname fhash na nb sumbsz sumdelta
    else Lwt.return ()

let output_rel_files e (bp : backup_plan) =
  let fbis = Env.consolidate_files e.fblocks in
  if !arg_dryrun then
    Lwt_list.iter_s (fun (fname, bis) -> let fhash = sha256 fname in
                       let%lwt () = validate_fileblocks fname fhash bis in
                       let%lwt () = Lwt_io.printlf "%s" fhash in
                       Lwt_list.iter_s (fun (fb : Assembly.blockinformation) ->
                                          Lwt_io.printlf "  %d@%d=%d %s" (Conversion.p2i fb.blockid)
                                                                         (Conversion.n2i fb.filepos)
                                                                         (Conversion.n2i fb.blocksize)
                                                                         fb.bchecksum
                                       )
                                       (List.rev bis)
                    ) fbis
  else
    let%lwt rel = Relfiles.new_map e.config in
    let%lwt () = Lwt_list.iter_s (fun (fname, bis) -> let fhash = sha256 fname in
                                    let%lwt () = validate_fileblocks fname fhash bis in
                                    let bpf = List.find (fun bpf -> bpf.fhash = fhash) bp.bp in
                                    let%lwt _rel' = Relfiles.add fhash {rfi=bpf.curfi; rfbs=bis} rel in Lwt.return ()) fbis in
    Relfiles.close_map rel

let output_rel_keys e =
  let%lwt rel = Relkeys.new_map e.config in
  let%lwt () = Lwt_list.iter_s (fun (aid, ki) ->
                                let%lwt _ = Relkeys.add aid ki rel in Lwt.return ()) e.keys in
  Relkeys.close_map rel

let output_relations e (bp : backup_plan) =
  let%lwt () = if !arg_dryrun then Lwt.return () else output_rel_keys e in
  output_rel_files e bp


let main () = Arg.parse argspec anon_args_fun "lxr_backup: vyxdnji";
  let nchunks = Nchunks.from_int !arg_nchunks in
  if List.length !arg_files > 0
  && !arg_nproc > 0 && !arg_nproc < 65
  then
    let myid = !arg_myid in
    let conf : configuration = {
                  config_nchunks = nchunks;
                  path_chunks = !arg_chunkpath;
                  path_db     = !arg_dbpath;
                  my_id       = myid } in
    let e0 = Environment.initial_environment conf in
    let%lwt backup_plan = plan_backup e0 !arg_files in
    let fcount = List.map (fun bpf -> bpf.fhash) backup_plan.bp |> List.sort_uniq (compare) |> List.length
    and acount = List.map (fun bpf -> bpf.curbs) backup_plan.bp |> List.map (List.map BackupPlanner.fbanum) |>
                              List.flatten |> List.sort_uniq (compare) |> List.length in
    let%lwt () = Lwt_io.printlf "backup of %d %s in %d %s" fcount (Utils.pluralise "file" fcount) acount (Utils.pluralise2 "assembly" "assemblies" acount) in
    let%lwt () = let rcount = List.map (fun bpf -> match bpf.refbs with None -> [] | Some ls -> ls) backup_plan.bp |>
                              List.map (List.map Assembly.blockaid) |>
                              List.flatten |> List.sort_uniq (compare) |> List.length in
                 Lwt_io.printlf "reference contains %d %s" rcount (Utils.pluralise2 "assembly" "assemblies" rcount) in
    let e1 = run_distributed_backup e0 !arg_nproc acount backup_plan in
    let e2 = Environment.finalise_assembly e1 in
    let%lwt () = output_relations e2 backup_plan in
    let%lwt () = Lwt_io.printl "done." in
    Lwt.return ()
  else
    Lwt.return ()

let () = Lwt_main.run (main ())
