(**
      e L y K s e e R
*)

Module Export Environment.

Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List.

Open Scope positive_scope.
Open Scope N_scope.

From LXR Require Import Assembly.
From LXR Require Import Block.
From LXR Require Import Configuration.
From LXR Require Import Filetypes.

Section axioms.

Axiom rndsetup : N -> N.

End axioms.

Record environment : Type :=
    mkenvironment
        { cur_assembly : assembly
        ; count_input_bytes : N
        ; config : configuration
        ; files : list fileblocks
        ; assemblies : list assembly
        }.

(** call this function to setup the random number generator to a fresh state *)
Definition setup_environment : N :=
    rndsetup 0%N.

Definition initial_environment (c : configuration) : environment :=
    let a := first_assembly (num_chunks c) in
    {| cur_assembly := a
    ;  count_input_bytes := 0%N
    ;  config := c
    ;  files := nil
    ;  assemblies := nil
    |}.
Definition env_set_assembly (e : environment) (a : assembly) : environment :=
    let ah := cur_assembly e in
    let tail := if ((anum a) =? (anum ah))%positive
                then tl (assemblies e)
                else assemblies e in
    {| cur_assembly := a
    ;  count_input_bytes := count_input_bytes e
    ;  config := config e
    ;  files := files e
    ;  assemblies := a :: tail
    |}.
Definition env_add_file (e : environment) (fibs : fileblocks) : environment :=
    {| cur_assembly := cur_assembly e
    ;  count_input_bytes := count_input_bytes e + fsize (bfi fibs)
    ;  config := config e
    ;  files := fibs :: files e
    ;  assemblies := assemblies e
    |}.

End Environment.