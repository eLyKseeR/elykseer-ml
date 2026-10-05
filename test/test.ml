(* Alcotest.run exits the process by default; ~and_exit:false lets the
   Lwt tests run afterwards, a failure is still raised as Test_error *)
let plain_tests () =
  let open Alcotest in
  run ~and_exit:false "LXR Assembly" [
    TestAssembly.test;
  ]

let lwt_tests () =
  let open Alcotest_lwt in
  run "LXR Relkeys" [
    TestRelkeys.test;
  ]

let () =
  let () = plain_tests () in
  Lwt_main.run (lwt_tests ())
