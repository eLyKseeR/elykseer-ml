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

let test_mk_cid_subdir () =
  Alcotest.(check string) "last four chars" "ab/89" (Elykseer_base.Helper.mk_cid_subdir "0123456789ab");
  Alcotest.check_raises "too short" (Invalid_argument "mk_cid_subdir: id too short")
    (fun () -> ignore (Elykseer_base.Helper.mk_cid_subdir "abc"))

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
  Alcotest.check_raises "get_str wrong type" (Failure "n: unexpected type") (fun () -> ignore (Relutils.get_str "n" bs));
  Alcotest.check_raises "get_int wrong type" (Failure "o: unexpected type") (fun () -> ignore (Relutils.get_int "o" bs));
  Alcotest.check_raises "get_int not a number" (Failure "int_of_string") (fun () -> ignore (Relutils.get_int "s" bs));
  Alcotest.check_raises "get_int missing" Not_found (fun () -> ignore (Relutils.get_int "zz" bs));
  Alcotest.(check int) "get_obj" 1 (List.length (Relutils.get_obj "o" bs));
  Alcotest.(check int) "get_arr" 1 (List.length (Relutils.get_arr "a" bs));
  Alcotest.check_raises "get_arr wrong type" (Failure "s: unexpected type") (fun () -> ignore (Relutils.get_arr "s" bs))

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

(* Processor.read_block: a file that shrank since its blocks were planned
   must not be stored as a full block *)
let test_read_block () =
  let fn = Filename.temp_file "lxr_read_block" ".bin" in
  Out_channel.with_open_bin fn (fun oc -> output_string oc "0123456789");
  let read sz =
    match Cstdio.fopen fn Cstdio.read_mode with
    | None -> Alcotest.fail "fopen"
    | Some fptr ->
      let r = Processor.read_block fptr (block 0 sz) in
      ignore (Cstdio.fclose fptr);
      Option.map (fun b -> Conversion.n2i (Cstdio.BufferPlain.buffer_len b)) r in
  Alcotest.(check (option int)) "complete block" (Some 10) (read 10);
  Alcotest.(check (option int)) "short read" None (read 100);
  Sys.remove fn

(* Distribution: the FS sink and the retry scheduler *)

let rec mkdir_p d = if not (Sys.file_exists d) then (mkdir_p (Filename.dirname d); Sys.mkdir d 0o755)
let write_file fn n =
  mkdir_p (Filename.dirname fn);
  Out_channel.with_open_bin fn (fun oc -> output_string oc (String.make n 'x'))

let fs_sink local base =
  let smap = List.fold_left (fun acc (k, v) -> Distribution.SMap.add k v acc) Distribution.SMap.empty
      [("name", "t"); ("user", "*"); ("basepath", base)] in
  match Distribution.FSSink.init { (config ()) with path_chunks = local } smap with
  | Some s -> s
  | None -> Alcotest.fail "FSSink.init"

let copy_result = Alcotest.testable (fun ppf r -> Format.pp_print_string ppf (match r with
    | Distribution.Copied -> "Copied" | Present -> "Present"
    | Retry c -> "Retry " ^ string_of_int (Conversion.z2i c)
    | Failed c -> "Failed " ^ string_of_int (Conversion.z2i c))) (=)
let failed c = Distribution.Failed (Conversion.i2z c)

let test_fssink () =
  let open Alcotest in
  let local = Filename.temp_dir "lxr_local" "" and local2 = Filename.temp_dir "lxr_local2" ""
  and base = Filename.temp_dir "lxr_sink" "" in
  let k = "ab/cd/chunk1.lxr" and short = "ab/ce/short.lxr" in
  write_file (Filename.concat local k) 262144;
  write_file (Filename.concat local short) 100;
  let s = fs_sink local base in
  let put k = snd (Distribution.FSSink.put k s) in
  check copy_result "put copies" Copied (put k);
  check copy_result "put again: present" Present (put k);
  check copy_result "short chunk is refused" (failed (-1003)) (put short);
  check bool "short chunk not copied" false (Sys.file_exists (Filename.concat base short));
  check copy_result "missing chunk is refused" (failed (-1003)) (put "ab/cd/missing.lxr");
  (* an interrupted copy left a short file on the sink: it is not "present", it is replaced *)
  let k3 = "ab/cf/chunk3.lxr" in
  write_file (Filename.concat local k3) 262144;
  write_file (Filename.concat base k3) 100;
  check copy_result "put replaces a partial copy" Copied (put k3);
  check int "partial copy replaced" 262144 (Unix.stat (Filename.concat base k3)).st_size;
  Sys.remove (Filename.concat base k3);
  write_file (Filename.concat base "ef/01/chunk2.lxr") 262144;
  let ls n = List.sort compare (snd (Distribution.FSSink.list_n (Conversion.i2n n) s)) in
  check (list string) "list_n all" ["ab/cd/chunk1.lxr"; "ef/01/chunk2.lxr"] (ls 5);
  check int "list_n 1" 1 (List.length (ls 1));
  check (list string) "list_n 0" [] (ls 0);
  let s2 = fs_sink local2 base in
  let get k = snd (Distribution.FSSink.get k s2) in
  check copy_result "get copies" Copied (get k);
  check bool "got the chunk" true (Sys.file_exists (Filename.concat local2 k));
  check copy_result "get again: present" Present (get k);
  write_file (Filename.concat base "ab/cd/bad.lxr") 100;
  check copy_result "get of a wrong size" (failed (-1003)) (get "ab/cd/bad.lxr");
  check bool "wrong size copy removed" false (Sys.file_exists (Filename.concat local2 "ab/cd/bad.lxr"));
  check copy_result "get of a missing chunk" (failed (-1005)) (get "ab/cd/nope.lxr");
  write_file (Filename.concat local2 "ef/01/chunk2.lxr") 100;
  check copy_result "get replaces a partial local chunk" Copied (get "ef/01/chunk2.lxr");
  check int "partial local chunk replaced" 262144 (Unix.stat (Filename.concat local2 "ef/01/chunk2.lxr")).st_size;
  let gone = Filename.concat base "gone" in
  check copy_result "missing sink directory" (failed (-1001)) (snd (Distribution.FSSink.put k (fs_sink local gone)));
  check copy_result "get: a local chunk needs no sink" Present (snd (Distribution.FSSink.get k (fs_sink local gone)));
  (* restore: every chunk from one sink is enough *)
  let fin s k r : _ Distribution.final = { f_sink = s; f_key = k; f_attempts = S O; f_result = r } in
  let fs = [fin 1 "a" (failed (-1001)); fin 2 "a" Copied; fin 1 "b" Present] in
  check bool "restore ok with one dead sink" true (Distribution.restore_ok ["a"; "b"] fs);
  check bool "restore not ok without c" false (Distribution.restore_ok ["a"; "b"; "c"] fs);
  (* the keys are the layout of chunk_identifier_path *)
  let c = { (config ()) with path_chunks = local } in
  let n16 = Nchunks.from_int 16 in
  check (list string) "chunk keys match chunk_identifier_path"
    (List.map (fun i -> Assembly.chunk_identifier_path c "aid123" (Conversion.i2p i)) (List.init 16 (fun i -> i + 1))
     |> List.sort compare)
    (List.map (Filename.concat local) (Distribution.enumerate_chunk_keys c "aid123" n16) |> List.sort compare)

(* a fake world: the clock in ms, every copy takes 1 ms; script key attempt -> result *)
let fake_run pol script plan =
  let calls = Hashtbl.create 8 in
  let copy t _s k =
    let a = 1 + Option.value ~default:0 (Hashtbl.find_opt calls k) in
    Hashtbl.replace calls k a;
    (t + 1, script k a) in
  let now t = (t, Conversion.i2n t) in
  let sleep_until t due = let t' = max t (Conversion.n2i due) in (t', Conversion.i2n t') in
  let ((_, finals), log) = Distribution.run_plan (fun _ -> pol) copy now sleep_until 0 plan in
  (finals, log)

let policy ~attempts ~deadline : Distribution.retrypolicy =
  { max_attempts = Sinkconfig.nat_of_int attempts; base_delay_ms = Conversion.i2n 10;
    max_delay_ms = Conversion.i2n 40; deadline_ms = Conversion.i2n deadline }

let int_of_nat = Sinkconfig.int_of_nat

let test_scheduler () =
  let open Alcotest in
  let retry = Distribution.Retry (Conversion.i2z 503) in
  let script k a = match k with
    | "a" -> if a <= 2 then retry else Distribution.Copied
    | "b" -> retry
    | "c" -> failed 403
    | _ -> Distribution.Present in
  let outcome (finals, _) = List.map (fun (f : _ Distribution.final) -> (f.f_key, int_of_nat f.f_attempts, f.f_result)) finals
                            |> List.sort compare in
  let res = testable (fun ppf _ -> Format.pp_print_string ppf "<results>") (=) in
  let plan = [("s1", ["a"; "b"]); ("s2", ["c"; "d"])] in
  let r = fake_run (policy ~attempts:5 ~deadline:100000) script plan in
  check res "attempts and results"
    [("a", 3, Distribution.Copied); ("b", 5, failed 503); ("c", 1, failed 403); ("d", 1, Distribution.Present)]
    (outcome r);
  check bool "not ok" false (Distribution.distribution_ok (fst r));
  check bool "ok without b and c" true
    (Distribution.distribution_ok (fst (fake_run (policy ~attempts:5 ~deadline:100000) script [("s1", ["a"; "d"])])));
  (* attempt 1 at 0 ms, attempt 2 at 11 ms; the next one would be due at 32 ms, after the deadline of 25 ms *)
  let r = fake_run (policy ~attempts:5 ~deadline:25) script [("s1", ["b"])] in
  check res "deadline stops retries" [("b", 2, failed 503)] (outcome r);
  check bool "no attempt after the deadline" true
    (List.for_all (fun (v : _ Distribution.event) ->
         Conversion.n2i v.v_start <= Conversion.n2i v.v_first + 25) (snd r));
  check int "max_attempts 0 still tries once" 1
    (match outcome (fake_run (policy ~attempts:0 ~deadline:100000) script [("s1", ["b"])]) with
     | [(_, n, _)] -> n | _ -> -1)

let test =
  let open Alcotest in
  "LXR Utils",
  [
    test_case "read_block short read" `Quick test_read_block;
    test_case "consolidate files" `Quick test_consolidate_files;
    test_case "fsutils" `Quick test_fsutils;
    test_case "cid subdir" `Quick test_mk_cid_subdir;
    test_case "sink configuration" `Quick test_sinkconfig;
    test_case "sinks.json example" `Quick test_sinks_example;
    test_case "relutils" `Quick test_relutils;
    test_case "counting tracer" `Quick test_tracing;
    test_case "nchunks range" `Quick test_nchunks_spec;
    test_case "FS sink put/get/list_n" `Quick test_fssink;
    test_case "retry scheduler" `Quick test_scheduler;
  ]

let test_lwt =
  let open Alcotest_lwt in
  "LXR Relfiles",
  [
    test_case "add and find" `Quick test_relfiles_roundtrip;
  ]
