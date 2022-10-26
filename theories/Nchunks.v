(**
      e L y K s e e R
*)

Module Export Nchunks.

(**
 Module: Nchunks
 Description: the number of chunks is defined in [16,256].
 *)

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Require Import ZArith NArith PArith.
From Coq Require Import NArith.BinNat Lia.
Open Scope positive_scope.
(* Open Scope N_scope. *)

From LXR Require Import Conversion.

Definition max_n : positive := 256.
Definition min_n : positive := 16.

Module Private.
    Definition t := positive.
    Definition from_positive : positive -> t := fun n =>
        Pos.min max_n (Pos.max n min_n).
    Definition from_int : int -> t := fun i =>
        from_positive (Conversion.i2p i).
    Definition to_positive : t -> positive := fun x => x.
    Definition to_N : t -> N := fun x => Conversion.pos2N x.
End Private.

Definition t : Type := Private.t.
(* Print t. *)
Definition from_positive : positive -> Private.t := Private.from_positive.
Definition from_int : int -> Private.t := Private.from_int.
Definition to_positive : Private.t -> positive := Private.to_positive.
Definition to_N : Private.t -> N := Private.to_N.

End Nchunks.