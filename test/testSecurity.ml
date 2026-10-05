open Elykseer_utils

(* Pathutils: restored files stay below the output directory *)

let relpath = Alcotest.(result string string)

let ok_path fname expected () =
  Alcotest.check relpath fname (Ok expected) (Pathutils.restore_relpath fname)

let rejected fname () =
  Alcotest.(check bool) ("rejects " ^ fname) true
    (Result.is_error (Pathutils.restore_relpath fname))

let test_is_below () =
  Alcotest.(check bool) "same" true (Pathutils.is_below ~base:"/tmp/out" "/tmp/out");
  Alcotest.(check bool) "below" true (Pathutils.is_below ~base:"/tmp/out" "/tmp/out/a/b");
  Alcotest.(check bool) "base with slash" true (Pathutils.is_below ~base:"/tmp/out/" "/tmp/out/a");
  Alcotest.(check bool) "sibling prefix" false (Pathutils.is_below ~base:"/tmp/out" "/tmp/outside/a");
  Alcotest.(check bool) "outside" false (Pathutils.is_below ~base:"/tmp/out" "/etc/passwd")

(* Sinkcheck: transport and secrets of lxr_distribute *)

let test_is_local_host () =
  List.iter (fun h -> Alcotest.(check bool) h true (Sinkcheck.is_local_host h))
    ["localhost"; "LOCALHOST"; "127.0.0.1"; "127.1.2.3"; "::1"; "[::1]"];
  List.iter (fun h -> Alcotest.(check bool) h false (Sinkcheck.is_local_host h))
    ["minio.example.com"; "10.0.0.1"; "localhost.example.com"; "128.0.0.1"]

let test_check_protocol () =
  let ok = Alcotest.(check bool) in
  ok "https remote" true (Result.is_ok (Sinkcheck.check_protocol ~insecure:false ~host:"s3.example.com" "https"));
  ok "http local" true (Result.is_ok (Sinkcheck.check_protocol ~insecure:false ~host:"localhost" "http"));
  ok "http remote" false (Result.is_ok (Sinkcheck.check_protocol ~insecure:false ~host:"s3.example.com" "http"));
  ok "http remote insecure" true (Result.is_ok (Sinkcheck.check_protocol ~insecure:true ~host:"s3.example.com" "http"));
  ok "ftp" false (Result.is_ok (Sinkcheck.check_protocol ~insecure:true ~host:"localhost" "ftp"))

let test_resolve_secret () =
  let getenv = function "LXR_TEST_SECRET" -> Some "from-env" | _ -> None in
  let inline = `Assoc [("secret-key", `String "inline")] in
  let env = `Assoc [("secret-key-env", `String "LXR_TEST_SECRET"); ("secret-key", `String "inline")] in
  let unset = `Assoc [("secret-key-env", `String "LXR_TEST_UNSET")] in
  let check = Alcotest.(check (option string)) in
  check "inline" (Some "inline") (Sinkcheck.resolve_secret ~getenv inline "secret-key");
  check "env wins" (Some "from-env") (Sinkcheck.resolve_secret ~getenv env "secret-key");
  check "env unset" None (Sinkcheck.resolve_secret ~getenv unset "secret-key");
  check "missing" None (Sinkcheck.resolve_secret ~getenv (`Assoc []) "secret-key");
  check "not an object" None (Sinkcheck.resolve_secret ~getenv `Null "secret-key");
  Alcotest.(check bool) "inline is inline" true (Sinkcheck.has_inline_secret inline "secret-key");
  Alcotest.(check bool) "env is not inline" false (Sinkcheck.has_inline_secret env "secret-key")

let test_check_config_mode () =
  let fp = Filename.temp_file "lxr_sinks" ".json" in
  let check name expected ~insecure ~inline_secrets mode =
    Unix.chmod fp mode;
    Alcotest.(check bool) name expected
      (Result.is_ok (Sinkcheck.check_config_mode ~insecure ~inline_secrets fp)) in
  check "600 with secrets" true ~insecure:false ~inline_secrets:true 0o600;
  check "644 with secrets" false ~insecure:false ~inline_secrets:true 0o644;
  check "640 with secrets" false ~insecure:false ~inline_secrets:true 0o640;
  check "644 with secrets insecure" true ~insecure:true ~inline_secrets:true 0o644;
  check "644 without secrets" true ~insecure:false ~inline_secrets:false 0o644;
  Sys.remove fp;
  Alcotest.(check bool) "missing file" false
    (Result.is_ok (Sinkcheck.check_config_mode ~insecure:false ~inline_secrets:false fp))

(* Runner *)

let test =
  let open Alcotest in
  "LXR Security",
  [
    test_case "relpath plain" `Quick (ok_path "a/b" "a/b");
    test_case "relpath absolute" `Quick (ok_path "/home/x/f" "home/x/f");
    test_case "relpath dot" `Quick (ok_path "./a//b/." "a/b");
    test_case "relpath parent" `Quick (rejected "../x");
    test_case "relpath nested parent" `Quick (rejected "a/../../x");
    test_case "relpath empty" `Quick (rejected "");
    test_case "relpath root" `Quick (rejected "/");
    test_case "is below" `Quick test_is_below;
    test_case "local host" `Quick test_is_local_host;
    test_case "protocol" `Quick test_check_protocol;
    test_case "resolve secret" `Quick test_resolve_secret;
    test_case "config file mode" `Quick test_check_config_mode;
  ]
