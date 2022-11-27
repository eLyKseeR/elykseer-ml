(* open Elykseer__Lxr *)
open Elykseer_base
open Mlcpp_cstdio

module Testing = struct
  let add_content = fun bsrc sz pos btgt -> Assembly.add_content ~src:bsrc ~sz:sz ~pos:pos ~tgt:btgt

end

(* Tests *)

let test_add_content () =
  let msg = "testing" in
  let content = Cstdio.File.Buffer.init 32 (fun i -> if i < 7 then String.get msg i else '0') in
  let buffer = Cstdio.File.Buffer.create (16*256*1024) in
  Alcotest.(check int) "add content"
  4 (* == *) (Testing.add_content content 4 0 buffer)

  
(* Runner *)

let test =
  let open Alcotest in
  "LXR Assembly",
  [
    test_case "add content" `Quick test_add_content;
  ]