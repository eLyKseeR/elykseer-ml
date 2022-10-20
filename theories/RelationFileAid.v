(**
      e L y K s e e R
*)

From LXR Require Import Rel.

Module Export RelationFileAid <: REL.

(**
 Module: RelationFileAid
 Description: relates file names to assembly
 *)

From Coq Require Import Strings.String NArith PArith FMaps FSets.FMapAVL.
Open Scope string_scope.

Module Import M := FMapAVL.Make(String_as_OT).
(* Module Import M := FMapList.Make(String_as_OT). *)
(* Print M. *)

Record blockinformation : Type := mkblockinformation
    { blockid : positive
    ; bchecksum : string
    ; blocksize : N
    ; filepos : N
    ; blockaid : string
    ; blockapos : N
    }.

Definition key := M.key.

Definition elt := list blockinformation.

Definition Map : Type := M.t elt.

Definition new : Map := M.empty _.

Definition find (name : key) (rel : Map) : option elt := M.find name rel.

Definition add (name : key) (bis : elt) (rel : Map) : Map :=
    (* let entries :=
        match M.find name rel with
        | Some bis => bi :: bis
        | None => bi :: nil
        end in *)
     M.add name bis rel.

Section Tests.

Compute find "test.txt" (add "test.txt" ({| blockid := 1; bchecksum := "chk"; blocksize := 37; filepos := 0; blockaid := "abc97391af"; blockapos := 0 |} :: nil) new).

Example rel1 := (add "test.txt" ({| blockid := 1; bchecksum := "chk1"; blocksize := 37; filepos := 0; blockaid := "abc97391af"; blockapos := 100 |}
                              :: {| blockid := 2; bchecksum := "chk2"; blocksize := 437; filepos := 37; blockaid := "abc97391af"; blockapos := 137 |}
                              :: nil)
                 new).
Print rel1.

Compute elements rel1.

End Tests.

End RelationFileAid.
