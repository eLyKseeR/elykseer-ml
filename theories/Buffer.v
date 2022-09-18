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
(* Open Scope positive_scope. *)
Open Scope N_scope.

Record buffer : Type := mkbuffer
    { width : N
    ; height : N
    }.
Definition new_buffer (w h : N) : buffer :=
    {| width := w; height := h |}.

Definition set (b : buffer) (idx : N) (v : Z) : Prop :=
    idx < (width b) * (height b).

Definition get (b : buffer) (idx : N) : Prop :=
    idx < (width b) * (height b).

End Buffer.