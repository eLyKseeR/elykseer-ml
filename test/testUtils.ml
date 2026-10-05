open Elykseer__Lxr
open Elykseer_utils

let config ?(path_db = "/tmp/lxr_test_db") () : Configuration.configuration =
  { config_nchunks = Nchunks.from_int 16
  ; path_chunks = "/tmp/lxr_test_chunks"
  ; path_db
  ; my_id = "4242"
  ; trace = Tracer.nullTracer }

let block ?(aid = "aid1") fpos sz : Assembly.blockinformation =
  { blockid = Conversion.i2p 99; bchecksum = "c" ^ string_of_int fpos
  ; blocksize = Conversion.i2n sz; filepos = Conversion.i2n fpos
  ; blockaid = aid; blockapos = Conversion.i2n fpos }

(* Env.consolidate_files *)

let test_consolidate_files () =
  let bis = [ ("f2", block 0 10); ("f1", block 20 5); ("f1", block 0 10); ("f1", block 10 10) ] in
  let res = Env.consolidate_files bis in
  Alcotest.(check (list string)) "one entry per file, names in reverse order" ["f2"; "f1"] (List.map fst res);
  let f1 = List.assoc "f1" res in
  (* blocks are numbered by file position, the list is in reverse order *)
  Alcotest.(check (list int)) "file positions" [20; 10; 0]
    (List.map (fun (b : Assembly.blockinformation) -> Conversion.n2i b.filepos) f1);
  Alcotest.(check (list int)) "block ids" [3; 2; 1]
    (List.map (fun (b : Assembly.blockinformation) -> Conversion.p2i b.blockid) f1);
  Alcotest.(check int) "empty" 0 (List.length (Env.consolidate_files []))

(* Fsutils *)

let with_file content f =
  let fp = Filename.temp_file "lxr_fsutils" ".bin" in
  Out_channel.with_open_bin fp (fun oc -> output_string oc content);
  Fun.protect ~finally:(fun () -> Sys.remove fp) (fun () -> f fp)

let test_fsutils () =
  with_file "hello, world" (fun fp ->
    Unix.chmod fp 0o640;
    Alcotest.(check int) "fsize" 12 (Elykseer_base.Fsutils.fsize fp);
    Alcotest.(check int) "fperm as octal digits" 640 (Elykseer_base.Fsutils.fperm fp);
    Unix.chmod fp 0o7;
    Alcotest.(check int) "fperm small" 7 (Elykseer_base.Fsutils.fperm fp);
    Unix.chmod fp 0o600;
    Alcotest.(check int) "fowner" (Unix.getuid ()) (Elykseer_base.Fsutils.fowner fp);
    Unix.utimes fp 0.0 86400.0;
    Alcotest.(check string) "fmod in UTC" "1970-01-02 00:00:00" (Elykseer_base.Fsutils.fmod fp);
    Alcotest.(check string) "fchksum" (Elykseer_crypto.Sha3_256.string "hello, world") (Elykseer_base.Fsutils.fchksum fp))

(* Sinkconfig *)

let sinks_json ?(protocol = "https") ?(host = "s3.example.com") () =
  Printf.sprintf {|{ "sinks": [
    { "type": "S3", "name": "s3",
      "credentials": { "access-key": "ak", "secret-key-env": "LXR_TEST_SECRET_UNSET" },
      "access": { "bucket": "b", "prefix": "p", "host": "%s", "port": "443", "protocol": "%s" } },
    { "type": "FS", "name": "fs", "credentials": { "user": "*" }, "access": { "basepath": "/tmp" } },
    { "type": "S3", "name": "s3b",
      "credentials": { "access-key": "ak", "secret-key": "sk" },
      "access": { "bucket": "b", "host": "%s", "port": "443", "protocol": "%s" } },
    { "type": "FTP", "name": "ftp" } ] }|} host protocol host protocol
  |> Yojson.Basic.from_string

let kinds sinks = List.map (function
    | None -> "none"
    | Some (Distribution.FS _) -> "FS"
    | Some (Distribution.S3 _) -> "S3") sinks

let test_sinkconfig () =
  let c = config () in
  (* first sink: secret from unset env var; last: unknown type *)
  Alcotest.(check (list string)) "https"
    ["none"; "FS"; "S3"; "none"] (Sinkconfig.from_json_sinks ~insecure:false c (sinks_json ()) |> kinds);
  Alcotest.(check (list string)) "http to remote host is refused"
    ["none"; "FS"; "none"; "none"] (Sinkconfig.from_json_sinks ~insecure:false c (sinks_json ~protocol:"http" ()) |> kinds);
  Alcotest.(check (list string)) "http to remote host with insecure"
    ["none"; "FS"; "S3"; "none"] (Sinkconfig.from_json_sinks ~insecure:true c (sinks_json ~protocol:"http" ()) |> kinds);
  Alcotest.(check (list string)) "http to localhost"
    ["none"; "FS"; "S3"; "none"] (Sinkconfig.from_json_sinks ~insecure:false c (sinks_json ~protocol:"http" ~host:"localhost" ()) |> kinds);
  Alcotest.(check (list string)) "no sinks" [] (Sinkconfig.from_json_sinks ~insecure:false c (`Assoc []) |> kinds);
  Alcotest.(check bool) "inline secrets" true (Sinkconfig.has_inline_secrets (sinks_json ()));
  Alcotest.(check bool) "no inline secrets" false
    (Sinkconfig.has_inline_secrets (Yojson.Basic.from_string
       {|{ "sinks": [ { "credentials": { "access-key-env": "A", "secret-key-env": "S" } } ] }|}))

let test_sinks_example () =
  (* the example configuration in the repository parses *)
  let j = Yojson.Basic.from_file "../sinks.json" in
  Alcotest.(check int) "three sinks" 3
    (Sinkconfig.from_json_sinks ~insecure:false (config ()) j |> List.length)

(* Relutils *)

let test_relutils () =
  let bs = [ ("i", `String "42"); ("s", `String "str"); ("o", `O [("k", `String "v")]); ("a", `A [`String "x"]); ("n", `Null) ] in
  Alcotest.(check int) "get_int" 42 (Relutils.get_int "i" bs);
  Alcotest.(check string) "get_str" "str" (Relutils.get_str "s" bs);
  Alcotest.(check string) "get_str fallback" "unk" (Relutils.get_str "n" bs);
  Alcotest.(check int) "get_obj" 1 (List.length (Relutils.get_obj "o" bs));
  Alcotest.(check int) "get_arr" 1 (List.length (Relutils.get_arr "a" bs));
  Alcotest.(check int) "get_arr fallback" 0 (List.length (Relutils.get_arr "s" bs))

(* Tracing *)

let test_tracing () =
  let (t, count) = Tracing.counting Tracer.nullTracer in
  ignore (Tracer.log t Tracer.Coq_debug "d");
  ignore (Tracer.log t Tracer.Coq_info "i");
  Alcotest.(check int) "debug and info not counted" 0 (count ());
  ignore (Tracer.log t Tracer.Coq_warning "w");
  ignore (Tracer.log t Tracer.Coq_error "e");
  Alcotest.(check int) "warnings and errors counted" 2 (count ())

(* Relfiles: add and find in an irmin store *)

let test_relfiles_roundtrip _ () =
  let path_db = Filename.temp_dir "lxr_test_relfiles" "" in
  let c = config ~path_db () in
  let fi : Filesupport.fileinformation =
    { fname = "dir/file"; fhash = "abcdef0123456789"; fsize = Conversion.i2n 30
    ; fowner = "1000"; fpermissions = Conversion.i2n 640; fmodified = "2026-10-05 12:00:00"
    ; fchecksum = "chk" } in
  let rel0 : Relfiles.relation = { rfi = fi; rfbs = [block 0 10; block ~aid:"aid2" 10 20] } in
  let%lwt db = Relfiles.new_map c in
  let%lwt db = match%lwt Relfiles.add fi.fhash rel0 db with
    | Ok db -> Lwt.return db
    | Error msg -> Alcotest.fail msg in
  let%lwt found = Relfiles.find fi.fhash db in
  let%lwt missing = Relfiles.find "0000000000000000" db in
  let%lwt hs = Relfiles.hashes db in
  Alcotest.(check (list string)) "hashes" [fi.fhash] hs;
  let%lwt () = Relfiles.close_map db in
  ignore (Sys.command (Filename.quote_command "rm" ["-rf"; path_db]));
  Alcotest.(check bool) "unknown hash" true (Option.is_none missing);
  match found with
  | None -> Alcotest.fail "relation not found"
  | Some r ->
    Alcotest.(check string) "fname" fi.fname r.rfi.fname;
    Alcotest.(check int) "fsize" 30 (Conversion.n2i r.rfi.fsize);
    Alcotest.(check string) "fchecksum" fi.fchecksum r.rfi.fchecksum;
    Alcotest.(check (list string)) "block aids" ["aid1"; "aid2"]
      (List.map (fun (b : Assembly.blockinformation) -> b.blockaid) r.rfbs);
    Alcotest.(check (list int)) "block sizes" [10; 20]
      (List.map (fun (b : Assembly.blockinformation) -> Conversion.n2i b.blocksize) r.rfbs);
    Lwt.return ()

(* Cli: range of -n *)

let test_nchunks_spec () =
  Alcotest.(check (pair int int)) "bounds" (16, 256) (Cli.min_nchunks, Cli.max_nchunks);
  let parse n =
    let r = ref 0 in
    match Arg.parse_argv ~current:(ref 0) [| "prog"; "-n"; n |] [("-n", Cli.nchunks_spec r, "")] ignore "" with
    | () -> Some !r
    | exception Arg.Bad _ -> None in
  Alcotest.(check (list (option int))) "accepted and rejected"
    [None; None; Some 16; Some 144; Some 256; None]
    (List.map parse ["-1"; "15"; "16"; "144"; "256"; "257"])

(* Runners *)

let test =
  let open Alcotest in
  "LXR Utils",
  [
    test_case "consolidate files" `Quick test_consolidate_files;
    test_case "fsutils" `Quick test_fsutils;
    test_case "sink configuration" `Quick test_sinkconfig;
    test_case "sinks.json example" `Quick test_sinks_example;
    test_case "relutils" `Quick test_relutils;
    test_case "counting tracer" `Quick test_tracing;
    test_case "nchunks range" `Quick test_nchunks_spec;
  ]

let test_lwt =
  let open Alcotest_lwt in
  "LXR Relfiles",
  [
    test_case "add and find" `Quick test_relfiles_roundtrip;
  ]
