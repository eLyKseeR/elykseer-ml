(* low-level S3 access for the S3 sink in theories/Distribution.v (s3_* axioms).
   Every function makes one synchronous request and returns the HTTP status code,
   or a negated curl error code; the decisions are made in Coq. *)

let extract_s3_region host =
  let ls = String.split_on_char '.' host in
  if List.length ls > 2 then
    List.rev ls |> List.to_seq |> Seq.drop 2 |> List.of_seq |> List.hd
  else
    "us-east-1"

(* requests are signed with AWS SigV4 by curl, using the access and secret key
   that are set as username and password on each request; one client per region *)
let clients : (string, Ezcurl.t) Hashtbl.t = Hashtbl.create 2
let client_for host =
  let region = extract_s3_region host in
  match Hashtbl.find_opt clients region with
  | Some c -> c
  | None ->
    let c = Ezcurl.make ~set_opts:(fun c ->
        Curl.set_aws_sigv4 c (Printf.sprintf "aws:amz:%s:s3" region);
        (* a hung connection must end, so that the retry scheduler gets its turn:
           30 s to connect, and give up below 1 byte/s for 60 s *)
        Curl.set_connecttimeout c 30;
        Curl.set_lowspeedlimit c 1;
        Curl.set_lowspeedtime c 60) () in
    Hashtbl.add clients region c; c

(* the one request helper for put, get and list *)
let request ~protocol ~host ~port ~user ~password ~meth ?content path =
  let url = Printf.sprintf "%s://%s:%s%s" protocol host port path in
  let config = Ezcurl.Config.default |>
               Ezcurl.Config.verbose false |> (* DEBUG: set verbose to true *)
               Ezcurl.Config.username user |>
               Ezcurl.Config.password password in
  match Ezcurl.http ~config ~client:(client_for host) ~headers:[] ~url ~meth ?content () with
  | Ok resp -> Ok resp
  | Error (code, msg) ->
    Printf.eprintf "error %d : %s\n" (Curl.int_of_curlCode code) msg;
    Error (- (Curl.int_of_curlCode code))

let is_2xx code = code >= 200 && code < 300

(* below the negated curl error codes; next to the local codes in theories/Distribution.v *)
let local_io_error = -1007

let put_object ~protocol ~host ~port ~user ~password ~resource ~file =
  match In_channel.with_open_bin file In_channel.input_all with
  | exception Sys_error msg -> Printf.eprintf "error reading chunk: %s\n" msg; local_io_error
  | data ->
    match request ~protocol ~host ~port ~user ~password ~meth:Ezcurl.PUT ~content:(`String data) resource with
    | Ok resp ->
      if not (is_2xx resp.code) then Printf.eprintf "error HTTP %d for %s\n" resp.code resource;
      resp.code
    | Error code -> code

(* writes the file only on an HTTP success, and never over an existing one *)
let get_object ~protocol ~host ~port ~user ~password ~resource ~file =
  match request ~protocol ~host ~port ~user ~password ~meth:Ezcurl.GET resource with
  | Error code -> code
  | Ok resp when not (is_2xx resp.code) ->
    Printf.eprintf "error HTTP %d for %s\n" resp.code resource;
    resp.code
  | Ok resp ->
    let open Mlcpp_filesystem.Filesystem in
    let _ = create_directories (Path.parent (Path.from_string file)) in
    let existed = Sys.file_exists file in
    match Out_channel.with_open_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o666 file
            (fun oc -> Out_channel.output_string oc resp.body) with
    | () -> resp.code
    | exception Sys_error msg ->
      Printf.eprintf "error writing chunk: %s\n" msg;
      (* only remove what this call created *)
      if not existed then (try Sys.remove file with Sys_error _ -> ());
      local_io_error

(* the text of every <tag>..</tag> in s *)
let extract_all tag s =
  let o = "<" ^ tag ^ ">" and c = "</" ^ tag ^ ">" in
  let find sub from =
    let n = String.length sub and l = String.length s in
    let rec go i = if i + n > l then None else if String.sub s i n = sub then Some i else go (i + 1) in
    go from in
  let rec loop from acc =
    match find o from with
    | None -> List.rev acc
    | Some i ->
      let j = i + String.length o in
      match find c j with
      | None -> List.rev acc
      | Some k -> loop (k + String.length c) (String.sub s j (k - j) :: acc) in
  loop 0 []

(* shortcut: only the first page (1000 objects) of ListObjectsV2; follow NextContinuationToken when a sample over all chunks is needed *)
let list_objects ~protocol ~host ~port ~user ~password ~prefix =
  let pre = if prefix = "" then "" else prefix ^ "/" in
  match request ~protocol ~host ~port ~user ~password ~meth:Ezcurl.GET ("/?list-type=2&prefix=" ^ pre) with
  | Ok resp when is_2xx resp.code ->
    let n = String.length pre in
    extract_all "Key" resp.body
    |> List.filter_map (fun k ->
        if String.length k > n && String.sub k 0 n = pre then Some (String.sub k n (String.length k - n)) else None)
  | Ok resp -> Printf.eprintf "error HTTP %d listing %s\n" resp.code pre; []
  | Error _ -> []
