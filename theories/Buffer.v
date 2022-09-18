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

From LXR Require Import Conversion.


Axiom buffer_t : Type.
Axiom buffer_create : N -> buffer_t.
Axiom buffer_len : buffer_t -> N.

Record buffer : Type := mkbuffer
    { len : N
    ; buf : buffer_t
    }.
Definition new_buffer (len : N) : buffer :=
    {| len := len; buf := buffer_create len |}.
Definition init_buffer (buf : buffer_t) : buffer :=
    {| len := buffer_len buf; buf := buf |}.

Definition set (b : buffer) (idx : N) (v : Z) : Prop :=
    idx < (len b).

Definition get (b : buffer) (idx : N) : Prop :=
    idx < (len b).

End Buffer.