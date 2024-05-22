(**
      e L y K s e e R
*)

(**
 Module: Conversion
 Description: conversion functions
 *)

From Coq Require Import NArith.BinNat Strings.String.

Module Export Conversion.

Definition pos2N (p : positive) : N :=
    Npos p.

Definition nat2N (i : nat) : N :=
    N.of_nat i.

Section axioms.
(** the following were found in: https://github.com/coq-contribs/zchinese 
    and are very useful to convert standard OCaml 'int' into positive|N|Z *)

Axiom int : Set.
Axiom i2p : int -> positive.
Axiom p2i : positive -> int.
Axiom i2z : int -> Z.
Axiom z2i : Z -> int.
Axiom i2n : int -> N.
Axiom n2i : N -> int.
Axiom i2s : int -> string.

End axioms.

End Conversion.
