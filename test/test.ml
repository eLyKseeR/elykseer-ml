
let plain_tests () =
  let open Alcotest in
  run "LXR Assembly" [
    TestAssembly.test;
  ]
let lwt_tests () =
  let open Alcotest_lwt in
  run "LXR Relkeys" [
    TestRelkeys.test;
  ]

let tests () =
  let () = plain_tests () in
  lwt_tests ()
 
let () =
  let () = plain_tests () in
  Lwt_main.run (tests ())
