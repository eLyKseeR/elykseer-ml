(**
      e L y K s e e R
*)

From Coq Require Import Strings.String Program.Basics.
Require Import NArith PArith.
From Coq Require Import NArith.BinNat.

From LXR Require Import Assembly Nchunks Conversion.

Module CheckApos.

(**
 Module: CheckApos
 Description: verifying properties of assembly indexing (apos) calculations.
 *)

Set Implicit Arguments.
Unset Strict Implicit.

Open Scope positive_scope.
Open Scope N_scope.

Definition chunkwidth  : positive := 256%positive.
Definition chunklength : positive := 1024%positive.
Definition chunksize   : positive := chunkwidth * chunklength.
Definition chunksize_N : N := Conversion.pos2N chunksize.
Definition assemblysize (n : Nchunks.Private.t) : N := chunksize_N * (to_N n).

Section Non_Local_Assembly.

(* a non-local apos calculation that fits in 32 bits *)
(* features: 
     - non-local by separating by n_chunks bytes
     - not touching first n_chunks bytes (reserved for random data)
     - index calculation fits in 32 bit *)
Local Definition idx2apos32 (idx : N) (a : assemblyinformation) : N :=
    let nch : N := Nchunks.to_N (nchunks a) in
    let asize : N := assemblysize (nchunks a) in
    let eff_asize : N := asize - nch in
    let proj : N := idx * nch in
    (proj mod eff_asize) + (N.div proj eff_asize) + nch.

(* a non-local apos calculation that requires 64 bits *)
(* features: 
     - non-local by separating by (chunkwidth * chunkheight - 1) bytes
     - not touching first n_chunks bytes (reserved for random data)
     - index calculation requires 64 bit *)
     Local Definition idx2apos64 (idx : N) (a : assemblyinformation) : N :=
     let nch : N := Nchunks.to_N (nchunks a) in
     let asize : N := assemblysize (nchunks a) in
     let eff_asize : N := asize - nch in
     let proj : N := idx * (chunksize_N - 1) in
     (proj mod eff_asize) + (N.div proj eff_asize) + nch.

Example apos32_of_index_0 :
    let ai := mkassembly (Nchunks.from_positive 17) "none" 0 in
    idx2apos32 0 ai = 17.
Proof.
    intros. eauto.
Qed.
Example apos64_of_index_0 :
    let ai := mkassembly (Nchunks.from_positive 17) "none" 0 in
    idx2apos64 0 ai = 17.
Proof.
    intros. eauto.
Qed.
Example apos32_separation_equal_nchunks :
    let ai := mkassembly (Nchunks.from_positive 17) "none" 0 in
    idx2apos32 1 ai - idx2apos32 0 ai = 17.
Proof.
    intros. eauto.
Qed.
Example apos64_separation_equal_chunksize_m1 :
    let ai := mkassembly (Nchunks.from_positive 17) "none" 0 in
    idx2apos64 1 ai - idx2apos64 0 ai = chunksize_N - 1.
Proof.
    intros. eauto.
Qed.

Local Fixpoint rec_bits_of (fuel : nat) (n : N) (b : N) : N :=
    match fuel with
    | O => b
    | S fuel' =>
        if N.eqb n 0 then
            b
        else
            rec_bits_of fuel' (N.div n 2) (b + 1)
    end.

Local Definition bits_of (n : N) : N :=
    rec_bits_of 128 n 0.

Example bits_of_0 :
    bits_of 0 = 0.
Proof.
    intros. unfold bits_of.
    simpl. reflexivity.
Qed.
Example bits_of_1 :
    bits_of 1 = 1.
Proof.
    intros. eauto.
Qed.
Example bits_of_2 :
    bits_of 2 = 2.
Proof.
    intros. eauto.
Qed.
Example bits_of_1023 :
    bits_of 1023 = 10.
Proof.
    intros. eauto.
Qed.
Example bits_of_1025 :
    bits_of 1025 = 11.
Proof.
    intros. unfold bits_of. simpl. reflexivity.
Qed.

(* index calculation idx2apos32 with n_chunks <= 128 is fitting into 32 bits *)
Example nchunks_128_max_bits_of_idx2apos32_eq_32 :
    let nch : N := 128 in
    let asize : N := chunksize_N * nch in
    let eff_asize : N := asize - nch in
    let proj : N := (eff_asize - 1) * nch in
    bits_of proj = 32.
Proof.
    intros. unfold bits_of.
    simpl. reflexivity.
Qed.

(* index calculation idx2apos32 with n_chunks > 128 does not fit into 32 bits *)
Example nchunks_129_max_bits_of_idx2apos32_eq_33 :
    let nch : N := 129 in
    let asize : N := chunksize_N * nch in
    let eff_asize : N := asize - nch in
    let proj : N := (eff_asize - 1) * nch in
    bits_of proj = 33.
Proof.
    intros. unfold bits_of.
    simpl. reflexivity.
Qed.

(* index calculation idx2apos64 with n_chunks <= 256 is fitting into 64 bits *)
(* the first n_chunks value to exceed 64 bits index calculation: 268437506,
   which would allow an assembly of 64 TB size *)
Example nchunks_128_max_bits_of_idx2apos64_eq_43 :
    let nch : N := 128 in
    let asize : N := chunksize_N * nch in
    let eff_asize : N := asize - nch in
    let proj : N := (eff_asize - 1) * (chunksize_N - 1) in
    bits_of proj = 43.
Proof.
    intros. unfold bits_of.
    simpl. reflexivity.
Qed.
Example nchunks_256_max_bits_of_idx2apos64_eq_44 :
    let nch : N := 256 in
    let asize : N := chunksize_N * nch in
    let eff_asize : N := asize - nch in
    let proj : N := (eff_asize - 1) * (chunksize_N - 1) in
    bits_of proj = 44.
Proof.
    intros. unfold bits_of.
    simpl. reflexivity.
Qed.

End Non_Local_Assembly.

End CheckApos.
