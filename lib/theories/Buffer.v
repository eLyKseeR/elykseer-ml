(**
      e L y K s e e R
*)

Module Export Buffer.

(**
 Module: Buffer
 Description: data buffer of specified length
 *)

From Coq Require Import ssreflect ssrfun ssrbool.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

From Coq Require Import Lia.
Require Import ZArith NArith.
Open Scope positive_scope.

Record buffer : Type := mkbuffer
    { width : positive
    ; height : positive
    }.
Definition new_buffer (w h : positive) : buffer :=
    {| width := w; height := h |}.

Definition set (b : buffer) (idx : positive) (v : positive) : Prop :=
    idx < (width b) * (height b).

Definition get (b : buffer) (idx : positive) : Prop :=
    idx < (width b) * (height b).

End Buffer.