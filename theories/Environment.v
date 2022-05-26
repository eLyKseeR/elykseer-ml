(**
      e L y K s e e R
*)

Module Export Environment.

Require Import NArith.
From Coq Require Import NArith.BinNat.
Open Scope N_scope.

From LXR Require Import Assembly.
From LXR Require Import Block.
From LXR Require Import Configuration.
From LXR Require Import Filetypes.

Record environment : Type :=
    mkenvironment
        { cur_assembly : assembly
        ; count_input_bytes : N
        ; files : list fileblocks
        }.
Definition initial_environment (c : configuration) : environment :=
    {| cur_assembly := first_assembly (num_chunks c)
    ;  count_input_bytes := 0%N
    ;  files := nil
    |}.
Definition env_set_assembly (e : environment) (a : assembly) : environment :=
    {| cur_assembly := a
    ;  count_input_bytes := count_input_bytes e
    ;  files := files e
    |}.
Definition env_add_file (e : environment) (fibs : fileblocks) : environment :=
    {| cur_assembly := cur_assembly e
    ; count_input_bytes := count_input_bytes e + fsize (bfi fibs)
    ; files := fibs :: files e
    |}.

End Environment.