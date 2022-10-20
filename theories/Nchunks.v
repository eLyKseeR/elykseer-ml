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
From Coq Require Import NArith.BinNat.
Open Scope positive_scope.
Open Scope N_scope.

From LXR Require Import Conversion.

Module Private.
    Definition t := positive.
    Definition from_positive : positive -> t := fun n =>
        Pos.min 256 (Pos.max n 16).
    Definition from_int : int -> t := fun i =>
        from_positive (Conversion.i2p i).
    Definition to_positive : t -> positive := fun x => x.
    Definition to_N : t -> N := fun x => Conversion.pos2N x.
End Private.

Definition from_positive : positive -> Private.t := Private.from_positive.
Definition from_int : int -> Private.t := Private.from_int.
Definition to_positive : Private.t -> positive := Private.to_positive.
Definition to_N : Private.t -> N := Private.to_N.

End Nchunks.