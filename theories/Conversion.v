(**
      e L y K s e e R
*)

Module Export Conversion.

(**
 Module: Conversion
 Description: conversion functions
 *)

From Coq Require Import Lia.
Require Import ZArith NArith PArith.
From Coq Require Import NArith.BinNat.
Open Scope positive_scope.
Open Scope N_scope.

Definition pos2N (p : positive) : N :=
    Npos p.

End Conversion.
