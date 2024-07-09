(* [@@@warning "-32"] *)
(* [@@@warning "-33"] *)

(* open Elykseer *)
open Elykseer__Lxr
open Elykseer__Lxr.Configuration
(* open Elykseer__Lxr.Environment *)

open Elykseer_utils

open Mlcpp_cstdio
open Mlcpp_filesystem

open Lwt.Infix

(* example: 
   dune exec bin/lxr_distribute.exe -- -v -d PUT -n 16 -x ../elykseer.chunks -i $MYID -a $AID -c sinks.json 4 99
*)
let def_myid = "1234567890"

let arg_verbose = ref false
let arg_weights : int list ref = ref []
let arg_aid = ref "<tbd>"
let arg_chunkpath = ref "lxr"
let arg_configpath = ref ""
let arg_nchunks : int ref = ref 16
let arg_myid = ref def_myid
let arg_direction = ref "PUT"
let arg_parallel = ref 1

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-d", Arg.Set_string arg_direction, "sets direction of copy: GET | PUT (default)");
    ("-a", Arg.Set_string arg_aid, "sets assembly id");
    ("-x", Arg.Set_string arg_chunkpath, "sets path for encrypted chunks");
    ("-n", Arg.Set_int arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
    ("-j", Arg.Set_int arg_parallel, "sets number of parallel jobs (default is 1)");
    ("-c", Arg.Set_string arg_configpath, "sets path for sink configuration file");
  ]

let anon_args_fun fn = arg_weights := (int_of_string fn) :: !arg_weights

let from_json_fs_sink c nm cs ac =
  match nm with
  | None -> None
  | Some name ->
    match Yojson.Basic.Util.member "user" cs |> Yojson.Basic.Util.to_string_option with
    | None -> None
    | Some user ->
      match Yojson.Basic.Util.member "basepath" ac |> Yojson.Basic.Util.to_string_option with
      | None -> None
      | Some basepath ->
        let smap = List.fold_left (fun acc (k,v) -> Distribution.SMap.add k v acc ) Distribution.SMap.empty  [("name", name); ("user", user); ("basepath", basepath)] in
        match Distribution.FSSink.init c smap with
        | None -> None
        | Some s -> Some (Distribution.FS s)

let from_json_s3_sink c nm cs ac =
  match nm with
  | None -> None
  | Some name ->
    match Yojson.Basic.Util.member "access-key" cs |> Yojson.Basic.Util.to_string_option with
    | None -> None
    | Some user ->
      match Yojson.Basic.Util.member "secret-key" cs |> Yojson.Basic.Util.to_string_option with
      | None -> None
      | Some password ->
        match Yojson.Basic.Util.member "bucket" ac |> Yojson.Basic.Util.to_string_option with
        | None -> None
        | Some bucket ->
          match Yojson.Basic.Util.member "host" ac |> Yojson.Basic.Util.to_string_option with
          | None -> None
          | Some host ->
            match Yojson.Basic.Util.member "port" ac |> Yojson.Basic.Util.to_string_option with
            | None -> None
            | Some port ->
              match Yojson.Basic.Util.member "protocol" ac |> Yojson.Basic.Util.to_string_option with
              | None -> None
              | Some protocol ->
                let smap = List.fold_left (fun acc (k,v) -> Distribution.SMap.add k v acc ) Distribution.SMap.empty  [("name", name); ("access", user); ("secret", password); ("bucket", bucket); ("protocol", protocol); ("host", host); ("port", port)] in
                match Distribution.S3Sink.init c smap with
                | None -> None
                | Some s -> Some (Distribution.S3 s)

let from_json_sink c s =
  let ty = Yojson.Basic.Util.member "type" s |> Yojson.Basic.Util.to_string_option
  and nm = Yojson.Basic.Util.member "name" s |> Yojson.Basic.Util.to_string_option
  and cs = Yojson.Basic.Util.member "credentials" s
  and ac = Yojson.Basic.Util.member "access" s in
  (* let () = Printf.printf "   sink: %s\n" (match nm with | None -> "??" | Some nm' -> nm') in *)
  match ty with
  | Some "FS" -> from_json_fs_sink c nm cs ac
  | Some "S3" -> from_json_s3_sink c nm cs ac
  | _ -> None

let from_json_sinks c j =
  match Yojson.Basic.Util.member "sinks" j with
  | `Null -> []
  | j' -> Yojson.Basic.Util.to_list j' |> List.map (from_json_sink c)

let read_256k_chunk fp =
  let bsz = 256 * 1024 in
  let fpath = Filesystem.Path.from_string fp in
  if Filesystem.Path.exists fpath && Filesystem.Path.file_size fpath = bsz then
    Cstdio.File.fopen fp "rx" |> function
    | Error err -> Error err
    | Ok file -> begin
      let b = Cstdio.File.Buffer.create bsz in
      Cstdio.File.fread b bsz file |> function
      | Error err -> Error err
      | Ok n -> begin
        let res = if n = bsz then Ok b
            else Error (-3, "size not read!") in
        Cstdio.File.fclose file |> function
        | Error err -> Error err
        | Ok () -> res
        end
    end
  else
    Error (-1, "file size is not 262144 bytes")

let mk_valid_bucket_path fp =
  String.split_on_char '/' fp |> List.rev |> List.to_seq |> Seq.take 3 |> List.of_seq |> List.rev
  |> List.fold_left (fun acc p -> acc ^ p ^ "/") ""
  |> fun s -> let l = String.length s in String.sub s 0 (l - 1)

let mk_s3_signature (dest : Distribution.S3Sink.coq_Sink) resource sdate meth =
  let content_type = "application/octet-stream" in
  let signature = Printf.sprintf "%s\n\n%s\n%s\n%s" meth content_type sdate resource in
  let hmac = Elykseer_crypto.Hmac.Sha1.string dest.creds.s3password signature in
  let k = Elykseer_crypto.Key160.from_hex hmac in
  Elykseer_crypto.Key160.to_bytes k |> Elykseer_crypto.Base64.encode

let copy_chunk_s3 (dest : Distribution.S3Sink.coq_Sink) fp =
  let odata = read_256k_chunk fp in
  match odata with
  | Error (code, msg) ->
    let%lwt () = Lwt_io.printlf "error reading chunk: %d %s" code msg in
    Lwt.return (fp, code)
  | Ok b ->
    let chunkdata = Cstdio.File.Buffer.to_string b in
    let resource = Printf.sprintf "/%s/%s" dest.connection.s3bucket (mk_valid_bucket_path fp) in
    let sdate : string = Http_date.(encode ~encoding:IMF (Ptime_clock.now ())) in
    let meth = !arg_direction in
    let signature = mk_s3_signature dest resource sdate meth in
    let config = Ezcurl_lwt.Config.default |> Ezcurl_lwt.Config.verbose false in  (* DEBUG: set verbose to true *)
    let headers = [("Host",dest.connection.s3host);("Date",sdate);("Content-type","application/octet-stream");("Authorization",Printf.sprintf "AWS %s:%s" dest.creds.s3user signature)] in
    let url = Printf.sprintf "%s://%s:%s%s" dest.connection.s3protocol dest.connection.s3host dest.connection.s3port resource in
    let curl_method = match meth with | "GET" -> Ezcurl_lwt.GET | "PUT" -> Ezcurl_lwt.PUT | _ -> Ezcurl_lwt.HEAD in
    Ezcurl_lwt.http ~config ~headers ~url ~meth:curl_method ~content: (`String chunkdata) () >>= fun response ->
      match response with
      | Ok resp ->
        let code = resp.code in
        let%lwt _ = Lwt_io.printlf "ok %d" code in
        Lwt.return (fp, code)
      | Error (code, msg) ->
        let%lwt _ = Lwt_io.printlf "error %d : %s" (Curl.int_of_curlCode code) msg in
        Lwt.return (fp, (Curl.int_of_curlCode code))

let copy_chunks_s3 (dest : Distribution.S3Sink.coq_Sink) fps =
  Lwt_list.mapi_s (fun _i fp -> copy_chunk_s3 dest fp) fps
let copy_chunks_fs _dest _fps = Lwt.return []

let copy_chunks os fps =
  match os with
  | Some (Distribution.FS s) -> copy_chunks_fs s fps
  | Some (Distribution.S3 s) -> copy_chunks_s3 s fps
  | _ -> Lwt.return []

(* main *)
let main () = Arg.parse argspec anon_args_fun "lxr_distribute: ";
  (* Printf.eprintf "weights: "; List.iter (fun w -> Printf.eprintf "%d " w) !arg_weights; Printf.eprintf "\n"; *)
  let nchunks = Nchunks.from_int !arg_nchunks in
  let myid = !arg_myid in
  let tracer = if !arg_verbose then Tracer.stdoutTracerDebug else Tracer.stdoutTracerWarning in
  let conf : configuration = {
                  config_nchunks = nchunks;
                  path_chunks = !arg_chunkpath;
                  path_db     = "/tmp/db";
                  my_id       = myid;
                  trace       = tracer } in
  let sinks = Yojson.Basic.from_file !arg_configpath |> from_json_sinks conf in
  let fps = Distribution.enumerate_chunk_paths conf !arg_aid nchunks in
  let ws = List.map (fun w -> Conversion.i2n w) (List.rev !arg_weights) in
  let fps' = Distribution.distribute_by_weight fps ws in
  let zipfps = Zip.zip sinks fps' in
  let%lwt res = Lwt_list.mapi_s
    (fun i (os,ls) -> let%lwt () = Lwt_io.printlf "%d : %s" (i + 1) (match os with | None -> "none" | Some (Distribution.FS s) -> "FS " ^ Distribution.name s | Some (Distribution.S3 s) -> "S3 " ^ Distribution.name s) in
     let%lwt () = Lwt_list.iteri_s (fun i fp -> Lwt_io.printlf "  %d: %s" (i + 1) fp) ls in
     copy_chunks os ls
    )
    zipfps  in
  Lwt_list.iteri_s (fun i lss ->
    let%lwt () = Lwt_io.printlf "%d : list of %d" i (List.length lss) in
    Lwt_list.iteri_s (fun j (fp, code) -> Lwt_io.printlf "   %d : %d %s" j code fp) lss )
    res

let () = Lwt_main.run (main ())