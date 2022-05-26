(**
      e L y K s e e R
*)

Module Export Block.

Require Import NArith.
From Coq Require Import NArith.BinNat.
(* From Coq Require Import Strings.String Strings.Byte. *)

Open Scope N_scope.

From LXR Require Import Filetypes.

(* Definition block := list byte. *)
Record blockinformation : Type :=
    mkblockinformation
        { blockid : positive
        ; blocksize : N
        ; filepos : N
        ; blockaid : positive
        ; blockapos : N
        }.

Record fileblocks : Type :=
    mkfileblocks
        { bfi : fileinformation
        ; blocks : list blockinformation
        }.

End Block.