(**
      e L y K s e e R
*)

Module Export Block.

Require Import NArith.
From Coq Require Import NArith.BinNat.
From Coq Require Import Strings.String.

Open Scope N_scope.

From LXR Require Import Filetypes.

(* Definition block := list byte. *)
Record blockinformation : Type :=
    mkblockinformation
        { blockid : positive
        ; bchecksum : string
        ; blocksize : N
        ; filepos : N
        ; blockanum : positive
        ; blockapos : N
        }.

Record fileblocks : Type :=
    mkfileblocks
        { bfi : fileinformation
        ; fversion : positive
        ; blocks : list blockinformation
        }.

End Block.