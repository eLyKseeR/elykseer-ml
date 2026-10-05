(* [@@@warning "-32"] *)
(* [@@@warning "-33"] *)

(* open Elykseer *)
open Elykseer__Lxr
open Elykseer__Lxr.Configuration

open Elykseer_utils

open Mlcpp_cstdio
open Mlcpp_filesystem

open Lwt.Infix

(* example: 
   dune exec bin/lxr_distribute.exe -- -v -d PUT -n 16 -x ../elykseer.chunks -i $MYID -a $AID -c sinks.json 4 99
*)

let arg_verbose = ref false
let arg_weights : int list ref = ref []
let arg_aid = ref ""
let arg_chunkpath = ref ""
let arg_configpath = ref ""
let arg_nchunks : int ref = ref 16
let arg_myid = ref Cli.default_myid
let arg_direction = ref "PUT"  (* or GET *)
let arg_insecure = ref false

let argspec =
  [
    ("-v", Arg.Set arg_verbose, "verbose output");
    ("-d", Arg.Set_string arg_direction, "sets direction of copy: GET | PUT (default)");
    ("-a", Arg.Set_string arg_aid, "sets assembly id");
    ("-x", Arg.Set_string arg_chunkpath, "sets path for encrypted chunks");
    ("-n", Cli.nchunks_spec arg_nchunks, "sets number of chunks (16-256) per assembly");
    ("-i", Arg.Set_string arg_myid, "sets own identifier");
    ("-c", Arg.Set_string arg_configpath, "sets path for sink configuration file");
    ("--insecure", Arg.Set arg_insecure, "allows http to non-local hosts and a sink configuration with inline secrets that is readable by group or others");
  ]

let usage_msg = "lxr_distribute -a aid -x chunkpath -c sinks.json [-d GET|PUT] [-n nchunks] [-i myid] [-v] [--insecure] <weight1> [<weight2>] ..."

let anon_args_fun fn =
  match int_of_string_opt fn with
  | Some w when w >= 0 -> arg_weights := w :: !arg_weights
  | _ -> raise (Arg.Bad (Printf.sprintf "weight must be a non-negative integer: '%s'" fn))

let chunksz = 256 * 1024

let is_256k_chunk fp =
  let fpath = Filesystem.Path.from_string fp in
  (Filesystem.Path.exists fpath) && (Filesystem.Path.file_size fpath = chunksz)

  let read_256k_chunk fp =
    if is_256k_chunk fp then
    Cstdio.File.fopen fp "rx" |> function
    | Error err -> Error err
    | Ok file -> begin
      let b = Cstdio.File.Buffer.create chunksz in
      Cstdio.File.fread b chunksz file |> function
      | Error err -> Error err
      | Ok n -> begin
        let res = if n = chunksz then Ok b
            else Error (-3, "size not read!") in
        Cstdio.File.fclose file |> function
        | Error err -> Error err
        | Ok () -> res
        end
    end
  else
    Error (-1, "file not found or size is not 262144 bytes")

let mk_valid_bucket_path fp =
  String.split_on_char '/' fp |> List.rev |> List.to_seq |> Seq.take 3 |> List.of_seq |> List.rev
  |> List.fold_left (fun acc p -> acc ^ p ^ "/") ""
  |> fun s -> let l = String.length s in String.sub s 0 (l - 1)

let extract_s3_region host =
  let ls = String.split_on_char '.' host in
  if List.length ls > 2 then
    List.rev ls |> List.to_seq |> Seq.drop 2 |> List.of_seq |> List.hd
  else
    "us-east-1"

(* result codes per chunk: 1 = copied, 0 = skipped (already present),
   200..299 = HTTP success; negative = failure (curl errors are negated,
   HTTP errors are positive >= 300) *)
let is_success code = code = 0 || code = 1 || (code >= 200 && code < 300)

let put_chunk_s3 client (dest : Distribution.S3Sink.coq_Sink) fp =
  let odata = read_256k_chunk fp in
  match odata with
  | Error (code, msg) ->
    let%lwt () = Lwt_io.printlf "error reading chunk: %d %s" code msg in
    Lwt.return (fp, code)
  | Ok b ->
    let chunkdata = Cstdio.File.Buffer.to_string b in
    let resource = Printf.sprintf "/%s/%s" dest.connection.s3prefix (mk_valid_bucket_path fp) in
    let meth = !arg_direction in
    let headers = [] in
    let url = Printf.sprintf "%s://%s:%s%s" dest.connection.s3protocol dest.connection.s3host dest.connection.s3port resource in
    let curl_method = match meth with | "GET" -> Ezcurl_lwt.GET | "PUT" -> Ezcurl_lwt.PUT | _ -> Ezcurl_lwt.HEAD in
    let config = Ezcurl_lwt.Config.default |>
                  Ezcurl_lwt.Config.verbose false |> (* DEBUG: set verbose to true *)
                  Ezcurl_lwt.Config.username dest.creds.s3user |>
                  Ezcurl_lwt.Config.password dest.creds.s3password
                 in
    Ezcurl_lwt.http ~config ~client ~headers ~url ~meth:curl_method ~content: (`String chunkdata) () >>= fun response ->
      match response with
      | Ok resp ->
        let code = resp.code in
        let%lwt _ = if !arg_verbose then Lwt_io.printlf "ok %d %s" code resp.body else Lwt.return_unit in
        Lwt.return (fp, code)
      | Error (code, msg) ->
        let%lwt _ = Lwt_io.printlf "error %d : %s" (Curl.int_of_curlCode code) msg in
        Lwt.return (fp, - (Curl.int_of_curlCode code))

let get_chunk_s3 client (dest : Distribution.S3Sink.coq_Sink) fp =
  let fpath = Filesystem.Path.from_string fp in
  if Filesystem.Path.exists fpath then
    let%lwt () = Lwt_io.printlf "chunk already exists: %s" fp in
    Lwt.return (fp, 0)
  else
    let resource0 = mk_valid_bucket_path fp in
    let resource = Printf.sprintf "/%s/%s" dest.connection.s3prefix resource0 in
    let meth = !arg_direction in
    let headers = [] in
    let url = Printf.sprintf "%s://%s:%s%s" dest.connection.s3protocol dest.connection.s3host dest.connection.s3port resource in
    let curl_method = match meth with | "GET" -> Ezcurl_lwt.GET | "PUT" -> Ezcurl_lwt.PUT | _ -> Ezcurl_lwt.HEAD in
    let config = Ezcurl_lwt.Config.default |>
                  Ezcurl_lwt.Config.verbose false |> (* DEBUG: set verbose to true *)
                  Ezcurl_lwt.Config.username dest.creds.s3user |>
                  Ezcurl_lwt.Config.password dest.creds.s3password
                 in
    Ezcurl_lwt.http ~config ~client ~headers ~url ~meth:curl_method () >>= fun response ->
      match response with
      | Ok resp when resp.code < 200 || resp.code >= 300 ->
        let%lwt _ = Lwt_io.printlf "error HTTP %d for %s" resp.code fp in
        Lwt.return (fp, resp.code)
      | Ok resp -> begin
        let code = resp.code in
        let%lwt _ = if !arg_verbose then Lwt_io.printlf "ok %d" code else Lwt.return_unit in
        let outp = Printf.sprintf "%s/%s" !arg_chunkpath resource0 in
        let%lwt _ = Lwt_io.printlf "outp %s" outp in
        match Cstdio.File.fopen outp "wx" with
        | Ok fptr -> begin
          match Cstdio.File.fwrite_s resp.body fptr with
          | Ok cnt -> begin
              Cstdio.File.fclose fptr |> ignore;
              if cnt = chunksz then
                Lwt.return (fp, 1)
              else begin
                (* do not leave a short chunk behind *)
                Sys.remove outp;
                Lwt.return (fp, -2)
              end
            end
          | Error (_, _msg) ->
            Cstdio.File.fclose fptr |> ignore;
            Sys.remove outp;
            Lwt.return (fp, -3)
          end
        | Error (_, msg) ->
          let%lwt _ = Lwt_io.printlf "error fopen : %s" msg in
          Lwt.return (fp, -4)
        end
      | Error (code, msg) ->
        let%lwt _ = Lwt_io.printlf "error %d : %s" (Curl.int_of_curlCode code) msg in
        Lwt.return (fp, - (Curl.int_of_curlCode code))


let get_chunk_fs (dest : Distribution.FSSink.coq_Sink) fp =
  let src = Filesystem.Path.from_string dest.connection in
  let tgt = Filesystem.Path.from_string fp in
  if Filesystem.Path.exists tgt then
    let%lwt () = Lwt_io.printlf "output path exists: %s" fp in
    Lwt.return (fp, 0)
  else
    if Filesystem.Path.is_directory src && not (Filesystem.Path.exists tgt) then
      let resource = Filesystem.Path.from_string (mk_valid_bucket_path fp) in
      let resp = Filesystem.Path.append src resource in
      if Filesystem.Path.exists resp then
        let _ = Filesystem.create_directories (Filesystem.Path.parent tgt) |> ignore in
        if Filesystem.copy_file resp tgt then
          Lwt.return (fp, 1)
        else
          let%lwt () = Lwt_io.printlf "failed to copy: %s -> %s" (Filesystem.Path.to_string resp) fp in
          Lwt.return (fp, -2)
      else
        let%lwt () = Lwt_io.printlf "source file does not exist: %s" (Filesystem.Path.to_string resp) in
        Lwt.return (fp, -5)
    else
      let%lwt () = Lwt_io.printlf "directory not found: %s" dest.connection in
      Lwt.return (fp, -3)

let put_chunk_fs (dest : Distribution.FSSink.coq_Sink) fp =
  let tgt = Filesystem.Path.from_string dest.connection in
  if Filesystem.Path.is_directory tgt then
    if not (is_256k_chunk fp) then
      let%lwt () = Lwt_io.printlf "error not a chunk file: %s" fp in
      Lwt.return (fp, -3)
    else
      let resource = Filesystem.Path.from_string (mk_valid_bucket_path fp) in
      let resp = Filesystem.Path.append tgt resource in
      if not (Filesystem.Path.exists resp) then
        let _ = Filesystem.create_directories (Filesystem.Path.parent resp) |> ignore in
        if Filesystem.copy_file (Filesystem.Path.from_string fp) resp then
          Lwt.return (fp, 1)
        else
          let%lwt () = Lwt_io.printlf "failed to copy: %s -> %s" fp (Filesystem.Path.to_string resp) in
          Lwt.return (fp, -2)
      else
        let%lwt () = Lwt_io.printlf "output file exists: %s" (Filesystem.Path.to_string resp) in
        Lwt.return (fp, 0)
  else
    let%lwt () = Lwt_io.printlf "directory not found: %s" dest.connection in
    Lwt.return (fp, -1)


(* requests are signed with AWS SigV4 by curl, using the access and secret key
   that are set as username and password on each request *)
let copy_chunks_s3 (dest : Distribution.S3Sink.coq_Sink) fps =
  let region = extract_s3_region dest.connection.s3host in
  let client = Ezcurl_lwt.make ~set_opts:(fun c -> Curl.set_aws_sigv4 c (Printf.sprintf "aws:amz:%s:s3" region)) () in
  if !arg_direction = "PUT" then
    Lwt_list.mapi_s (fun _i fp -> put_chunk_s3 client dest fp) fps
  else
    Lwt_list.mapi_s (fun _i fp -> get_chunk_s3 client dest fp) fps

let copy_chunks_fs (dest : Distribution.FSSink.coq_Sink) fps =
  if !arg_direction = "PUT" then
    Lwt_list.mapi_s (fun _i fp -> put_chunk_fs dest fp) fps
  else
    Lwt_list.mapi_s (fun _i fp -> get_chunk_fs dest fp) fps

let copy_chunks os fps =
  match os with
  | Some (Distribution.FS s) -> copy_chunks_fs s fps
  | Some (Distribution.S3 s) -> copy_chunks_s3 s fps
  | _ -> Lwt.return []

(* pairs up the elements of both lists, up to the length of the shorter one *)
let rec zip xs ys =
  match xs, ys with
  | x :: xs', y :: ys' -> (x, y) :: zip xs' ys'
  | _ -> []

let rec summarize_code c0 codes cnt res =
  match codes with
  | [] -> (c0,cnt) :: res
  | c :: r -> if c = c0 then
                 summarize_code c r (cnt + 1) res
              else
                 summarize_code c r 1 ((c0,cnt) :: res)

let summarize ls0 =
  let codes0 = List.map (fun (_, c) -> c) ls0 in
  let codes = List.sort compare codes0 in
  match codes with
  | [] -> []
  | c0 :: r ->
    summarize_code c0 r 1 []

  
(* main *)
let main () = Arg.parse argspec anon_args_fun usage_msg;
  Cli.require argspec usage_msg [("-a", !arg_aid); ("-x", !arg_chunkpath); ("-c", !arg_configpath)];
  if !arg_weights = [] then Cli.usage_error argspec usage_msg "missing weights: one per sink";
  if !arg_direction <> "PUT" && !arg_direction <> "GET" then Cli.usage_error argspec usage_msg "-d must be GET or PUT";
  Cli.warn_default_myid !arg_myid;
  (* Printf.eprintf "weights: "; List.iter (fun w -> Printf.eprintf "%d " w) !arg_weights; Printf.eprintf "\n"; *)
  let nchunks = Nchunks.from_int !arg_nchunks in
  let myid = !arg_myid in
  let tracer = if !arg_verbose then Tracer.stdoutTracerDebug else Tracer.stdoutTracerWarning in
  let conf : configuration = {
                  config_nchunks = nchunks;
                  path_chunks = !arg_chunkpath;
                  path_db     = Filename.concat (Filename.get_temp_dir_name ()) "db";
                  my_id       = myid;
                  trace       = tracer } in
  let jconf = Yojson.Basic.from_file !arg_configpath in
  let inline_secrets = Sinkconfig.has_inline_secrets jconf in
  let () = match Sinkcheck.check_config_mode ~insecure:!arg_insecure ~inline_secrets !arg_configpath with
    | Ok () -> ()
    | Error msg -> Printf.eprintf "%s\n" msg; exit 2 in
  let sinks = Sinkconfig.from_json_sinks ~insecure:!arg_insecure conf jconf in
  let () = if List.exists Option.is_none sinks then begin
      Printf.eprintf "invalid sink configuration in '%s'\n" !arg_configpath; exit 2 end in
  let fps = Distribution.enumerate_chunk_paths conf !arg_aid nchunks in
  let ws = List.map (fun w -> Conversion.i2n w) (List.rev !arg_weights) in
  let fps' = Distribution.distribute_by_weight fps ws in
  let zipfps = zip sinks fps' in
  let%lwt res = Lwt_list.mapi_s
    (fun i (os,ls) ->
      let%lwt () = Lwt_io.printlf "%d : %s" (i + 1) (match os with | None -> "none" | Some (Distribution.FS s) -> "FS " ^ Distribution.name s | Some (Distribution.S3 s) -> "S3 " ^ Distribution.name s) in
      let%lwt () = if !arg_verbose then
          Lwt_list.iteri_s (fun i fp -> Lwt_io.printlf "  %d: %s" (i + 1) fp) ls
        else Lwt.return_unit
      in
      let%lwt res' = copy_chunks os ls in
      let%lwt () = Lwt_list.iter_s (fun (code,cnt) -> Lwt_io.printlf "   code %d has count = %d" code cnt) (summarize res') in
      Lwt.return res'
    )
    zipfps  in
  let%lwt () = if !arg_verbose then
    Lwt_list.iteri_s (fun i lss ->
      let%lwt () = Lwt_io.printlf "%d : list of %d" i (List.length lss) in
      Lwt_list.iteri_s (fun j (fp, code) -> Lwt_io.printlf "   %d : %d %s" j code fp) lss )
      res
  else
    Lwt.return_unit in
  let nfailed = List.concat res |> List.filter (fun (_, code) -> not (is_success code)) |> List.length in
  let%lwt () = if nfailed > 0
    then Lwt_io.eprintlf "distribution incomplete: %d chunk%s failed" nfailed (if nfailed > 1 then "s" else "")
    else Lwt.return_unit in
  Lwt.return nfailed

let () = if Lwt_main.run (main ()) > 0 then exit 1