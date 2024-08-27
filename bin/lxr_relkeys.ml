
open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils

let def_myid = "1234567890"

let arg_verbose = ref false
let arg_files = ref []
let arg_dbpath = ref (Filename.concat (Filename.get_temp_dir_name ()) "db")
let arg_myid = ref def_myid

let usage_msg = "lxr_relkeys [-v] [-i myid] [-d dbpath] <file1> [<file2>] ..."

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-d", Arg.Set_string arg_dbpath, "sets database path");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
  ]

let anon_args_fun fn = arg_files := fn :: !arg_files


let filename2aidlist fn relfiles : string list Lwt.t =
  let%lwt rel = Relfiles.find (Elykseer_crypto.Sha3_256.string (fn ^ !arg_myid)) relfiles in
  match rel with
  | None -> Lwt.return []
  | Some r -> Lwt.return @@ List.map (fun (e : Assembly.blockinformation) -> e.blockaid) r.rfbs

let filenames2aidlist relfiles fns : string list Lwt.t =
  let%lwt l = Lwt_list.map_s (fun fn -> filename2aidlist fn relfiles) fns in
  Lwt.return (List.flatten l |> List.sort_uniq (compare))


let output_keys fns relfiles relkeys =
  let%lwt laid = filenames2aidlist relfiles fns in
  let%lwt lk = Lwt_list.fold_left_s (fun acc aid -> let%lwt k = Relkeys.find_v aid relkeys in Lwt.return ((aid,k) :: acc))
                          [] laid in
  let li = List.fold_left (fun acc (aid,e) -> match e with
                            | None ->
                                let () = if !arg_verbose
                                  then Format.printf "missing key for aid ='%s'\n" aid else () in
                                acc
                            | Some e' -> (aid,e') :: acc) [] lk in
  match li with
  | [] -> Lwt.return_unit
  | _ ->
    let%lwt () = Lwt_io.printl "\"myid\",\"nchunks\",\"version\",\"aid\",\"key\",\"iv\"" in
    Lwt_list.iter_s (fun ((aid,(version,ki)) : (string * (string * Assembly.keyinformation))) ->
                          Lwt_io.printlf "%s,%d,\"%s\",\"%s\",\"%s\",\"%s\""
                            ki.localid
                            (Conversion.p2i ki.localnchunks)
                            version aid ki.pkey ki.ivec ) li

(* main *)
let main () = Arg.parse argspec anon_args_fun usage_msg;
  if !arg_files != []
    then
      let nchunks = Nchunks.from_int 16 in
      let myid = !arg_myid in
      let conf : configuration = {
                      config_nchunks = nchunks;
                      path_chunks = "lxr";
                      path_db     = !arg_dbpath;
                      my_id       = myid;
                      trace       = Tracer.nullTracer } in
      let%lwt relfiles = Relfiles.new_map conf in
      let%lwt relkeys = Relkeys.new_map conf in
      output_keys !arg_files relfiles relkeys
    else Lwt.return ()

let () = Lwt_main.run (main ())
