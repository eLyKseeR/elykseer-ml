let () =
  let open Alcotest in
  run "LXR Assembly" [
    TestAssembly.test;
  ]