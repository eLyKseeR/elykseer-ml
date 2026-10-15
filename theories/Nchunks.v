(**
      e L y K s e e R
*)

From Stdlib Require Import ZArith NArith PArith.
From Stdlib Require Import NArith.BinNat Lia Program.

From LXR Require Import Conversion.

Module Export Nchunks.

(**
 Module: Nchunks
 Description: the number of chunks is defined in [16,256].
 *)

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Open Scope positive_scope.
(* Open Scope N_scope. *)

Definition max_n : positive := 256.
Definition min_n : positive := 16.

(* Dependent type: positive in range [min_n, max_n] with proof *)
Definition t := { p : positive | (Npos min_n <= Npos p)%N /\ (Npos p <= Npos max_n)%N }.

Program Definition from_positive (n : positive) : t :=
    exist _ (Pos.min max_n (Pos.max n min_n)) _.
Next Obligation.
    unfold min_n, max_n. lia.
Defined.

Definition from_int (i : int) : t := from_positive (Conversion.i2p i).

Definition to_positive (x : t) : positive := proj1_sig x.

Definition to_N (x : t) : N := Conversion.pos2N (proj1_sig x).

End Nchunks.