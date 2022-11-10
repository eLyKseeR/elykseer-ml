
(* open Elykseer *)
open Elykseer__Lxr
open Elykseer__Lxr.Configuration
(* open Elykseer__Lxr.Environment *)

open Elykseer_utils

open Mlcpp_cstdio
open Mlcpp_filesystem

let def_myid = 1234567890

let arg_verbose = ref false
let arg_files = ref []
let arg_metapath = ref "meta"
let arg_chunkpath = ref "lxr"
let arg_outpath = ref "/tmp/"
let arg_nchunks = ref 16
let arg_nproc = ref 1
let arg_ref1 = ref ""
let arg_ref2 = ref ""
let arg_ref3 = ref ""
let arg_myid = ref def_myid

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-o", Arg.Set_string arg_outpath, "sets output path for restored files");
    ("-x", Arg.Set_string arg_chunkpath, "sets path for encrypted chunks");
    ("-f", Arg.Set_string arg_metapath, "sets path for meta data");
    ("-n", Arg.Set_int arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-j", Arg.Set_int arg_nproc, "sets number of parallel processes");
    ("-r1", Arg.Set_string arg_ref1, "adds reference files .relkeys and .relfiles");
    ("-r2", Arg.Set_string arg_ref2, "adds reference files .relkeys and .relfiles");
    ("-r3", Arg.Set_string arg_ref3, "adds reference files .relkeys and .relfiles");
    ("-i", Arg.Set_int arg_myid, "sets own identifier (positive number)");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files

let mk_path fp =
  !arg_outpath ^ "/" ^ fp

let ensure_assembly e aid =
  if (Environment.cur_assembly e |> Assembly.aid) = aid
    then Ok (Environment.cur_assembly e, Environment.cur_buffer e)
  else 
  match RelationAidKey.find aid (Environment.keys e) with
  | None -> Error "no key found"
  | Some ki ->
    let config : Configuration.configuration = { config_nchunks = ki.localnchunks;
                   path_chunks = e.config.path_chunks; path_meta = e.config.path_meta;
                   my_id = ki.localid } in
    let ai : Assembly.assemblyinformation = { nchunks = ki.localnchunks; aid = aid; apos = Conversion.i2n 0 } in
    match Assembly.recall config ai with
    | None -> Error "cannot recall assembly"
    | Some (a,b) -> match Assembly.decrypt a b e.keys with
      | None -> Error "failed to decrypt"
      | Some (a',b') -> Ok (a',b')

let restore_file_block e0 fptr (fb : RelationFileAid.blockinformation) =
  ensure_assembly e0 fb.blockaid |> function
  | Error errstr -> Printf.printf "  failed to recall assembly %s with '%s'\n" fb.blockaid errstr; e0
  | Ok (a,b) ->
      let (_a'', b'') = Assembly.finish a b in
      let sz = Conversion.n2i fb.blocksize in
      let fbuf = Cstdio.File.Buffer.create sz in
      let abuf = Buffer.BufferPlain.to_buffer @@ Assembly.id_buffer_t_from_full b'' in
      let _nread = Elykseer_base.Assembly.get_content ~src:abuf ~sz:sz ~pos:(Conversion.n2i fb.blockapos) ~tgt:fbuf in
      Cstdio.File.fseek fptr (Conversion.n2i fb.filepos) |> function
        | Error _ -> Printf.printf "  failed to 'fseek'\n"; e0
        | Ok _ -> Cstdio.File.fwrite fbuf sz fptr |> function
          | Error _ -> Printf.printf "  failed to 'fwrite'\n"; e0
          | Ok _nwritten -> { e0 with cur_assembly=a; cur_buffer=b }

let restore_file e0 fname =
  let ofbs = RelationFileAid.find fname (Environment.files e0) in
  match ofbs with
  | None -> begin Printf.printf "  cannot restore file '%s'\n" fname; e0 end
  | Some fbs ->
    if !arg_verbose then Printf.printf "  restoring file '%s' from %d blocks\n" fname (List.length fbs) else ();
    let fout_path = mk_path fname in
    Cstdio.File.fopen fout_path "wx" |> function
    | Error (errno,errstr) -> Printf.printf "  fopen returnd: %d/%s\n" errno errstr; e0
    | Ok fptr ->
      let e1 = List.fold_left (fun env fb -> restore_file_block env fptr fb) e0 fbs in
      Cstdio.File.fclose fptr |> ignore;
      e1

let ensure_all_available e0 fns =
  let flen = List.length fns in
  let efiles = Environment.files e0 in
  let rlen = List.fold_left (fun acc fname -> match RelationFileAid.find fname efiles with
                              | Some _bis -> 1 + acc
                              | None -> acc
                            ) 0 fns in
  flen == rlen

let rec restore_files e0 fns =
  if ensure_all_available e0 fns then
    match fns with
      [] -> ()
    | fn :: fns' -> let e1 = restore_file e0 fn in
                    restore_files e1 fns'
  else Printf.printf "some information about files not found!" ;()

let merge_rel_files e0 fn =
  match Relfiles.load_from_file (Filesystem.Path.from_string fn) "testuser" (* TODO *) with
  | None -> e0
  | Some map ->
    List.fold_left (fun e (fname,bis) ->
      List.fold_left (Environment.env_add_file_block fname) e bis)
      e0 (RelationFileAid.M.elements map)

let merge_rel_keys e0 fn =
  match Relkeys.load_from_file (Filesystem.Path.from_string fn) "testuser" (* TODO *) with
  | None -> e0
  | Some map ->
    List.fold_left (fun e (aid,ki) -> Environment.env_add_aid_key aid e ki)
      e0 (RelationAidKey.M.elements map)

let load_reference e0 fbase =
  let e1 = merge_rel_keys e0 (!arg_metapath ^ "/" ^ fbase ^ ".relkeys") in
  let e2 = merge_rel_files e1 (!arg_metapath ^ "/" ^ fbase ^ ".relfiles") in
  if !arg_verbose then begin
    Printf.printf "   number of files: %d\n" (List.length (RelationFileAid.M.elements e2.files));
    Printf.printf "   number of keys: %d\n" (List.length (RelationAidKey.M.elements e2.keys))
    end
  else ();
  e2

let load_references e0 = 
  let e1 = if !arg_ref1 = "" then e0
           else load_reference e0 !arg_ref1 in
  let e2 = if !arg_ref2 = "" then e1
           else load_reference e1 !arg_ref2 in
  let e3 = if !arg_ref3 = "" then e2
           else load_reference e2 !arg_ref3 in
  e3

let exists_output_dir d =
  let dp = Filesystem.Path.from_string d in
  if Filesystem.Path.exists dp && Filesystem.Path.is_directory dp
    then true
    else begin
      Printf.printf "output directory '%s' does not exist or is not a directory\n" d;
      false
    end

(* main *)
let () = Arg.parse argspec anon_args_fun "lxr_restore: voxfnji";
         let nchunks = Nchunks.from_int !arg_nchunks in
         if List.length !arg_files > 0
          && !arg_nproc > 0 && !arg_nproc < 65
          && exists_output_dir !arg_outpath
         then
           let myid = let id0 = !arg_myid in
                      if id0 >= 0 then id0 else def_myid in
           let conf : configuration = {
                         config_nchunks = nchunks;
                         path_chunks = !arg_chunkpath;
                         path_meta   = !arg_metapath;
                         my_id       = Conversion.i2n myid } in
           let e0 = Environment.initial_environment conf in
           let e1 = load_references e0 in
           restore_files e1 !arg_files
