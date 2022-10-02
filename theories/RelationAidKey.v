(**
      e L y K s e e R
*)

From LXR Require Import Rel.

Module Export RelationAidKey <: REL.

(**
 Module: RelationAidKey
 Description: relates assembly to key
 *)

From Coq Require Import Strings.String NArith PArith FMaps.
Open Scope string_scope.

Module Import M := FMapList.Make(String_as_OT).
(* Print M. *)

Record keyinformation : Type := mkkeyinformation
    { pkey : string
    }.

Definition key := M.key.

Definition elt := keyinformation.

Definition Map : Type := M.t elt.

Definition new : Map := M.empty _.

Definition find (name : key) (rel : Map) : option elt := M.find name rel.

Definition add (name : key) (bi : elt) (rel : Map) : Map := M.add name bi rel.

Section Tests.

Definition rel1 := (add "none" {|pkey:="empty"|} (add "abc97391af" {| pkey := "guessme" |} new)).
Compute find "abc97391af" rel1.

End Tests.

End RelationAidKey.
