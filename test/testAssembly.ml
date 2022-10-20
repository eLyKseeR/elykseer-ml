open Elykseer__Lxr
open Elykseer_base
open Mlcpp_cstdio

module Testing = struct
  let add_content = fun bsrc sz pos btgt -> Assembly.add_content bsrc sz pos btgt

end

(* Tests *)

let test_add_content () =
  let msg = "testing" in
  let content = Cstdio.File.Buffer.init 32 (fun i -> if i < 7 then String.get msg i else '0') in
  let buffer = Cstdio.File.Buffer.create (16*256*1024) in
  Alcotest.(check int) "add content"
  4 (* == *) (Testing.add_content content 4 0 buffer)

let test_relation_aid_key () =
  let aid = "aid101" in
  let relkey = RelationAidKey.add aid "key101" @@ RelationAidKey.coq_new in
  Alcotest.(check string) "get key for aid"
  "key101" (* == *) (RelationAidKey.find aid relkey |> function
                    | None -> "<nothing>"
                    | Some k -> k
                    )
  
(* Runner *)

let test =
  let open Alcotest in
  "LXR Assembly",
  [
    test_case "add content" `Quick test_add_content;
    test_case "find key" `Quick test_relation_aid_key;
  ]