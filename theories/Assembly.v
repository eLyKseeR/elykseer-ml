(**
      e L y K s e e R
*)

 Module Export Assembly.

(**
 Module: Assembly
 Description: an assembly is an ordering of chunks of data,
              either plain for reading and writing, 
              or encrypted for longterm storage.
 *)

From Coq Require Import ssreflect ssrfun ssrbool.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

From Coq Require Import Strings.String Strings.Byte Lists.List Lia.
Require Import ZArith NArith PArith.
From Coq Require Import NArith.BinNat.
Open Scope positive_scope.
Open Scope N_scope.

From LXR Require Import Buffer Conversion.

(** constants *)
Section Constants.

(** chunk 2D size *)
Definition chunkwidth  : positive := 256%positive.
Definition chunklength : positive := 1024%positive.

(** assembly size range (count of chunks) *)
Definition assemblyminsz := 16%positive.
Definition assemblymaxsz := 256%positive.

End Constants.

Section Data.

(** data structures *)

(* Import BigNMatrix. *)

Definition valid_assembly_size (n : positive) : Prop :=
    n >= assemblyminsz /\ n <= assemblymaxsz.

Lemma invalid_assembly_low : forall n, n < assemblyminsz /\ valid_assembly_size n -> False.
Proof.
    intros n. unfold valid_assembly_size.
    unfold assemblyminsz. unfold assemblymaxsz.
    lia.
Qed.
Lemma invalid_assembly_hi : forall n, n > assemblymaxsz /\ valid_assembly_size n -> False.
Proof.
    intros n. unfold valid_assembly_size.
    unfold assemblyminsz. unfold assemblymaxsz.
    lia.
Qed.

(** encrypted data can be extracted as chunks 
    a chunk is unique per assembly
*)
Record chunk : Type := mkchunk
    { cid : positive
    ; in_aid : positive
    ; buffer : Buffer.buffer (*mkbuffer chunkwidth chunklength*)
    }.
    (* ; buffer : matrix (to_nat chunkwidth) (to_nat chunklength) *)
(* Print chunk. *)
Definition new_chunk (p_cid p_aid : positive) : chunk :=
    mkchunk p_cid p_aid (mkbuffer chunkwidth chunklength).

Definition equal_chunks (c1 c2 : chunk) : Prop :=
    cid c1 = cid c2 /\ in_aid c1 = in_aid c2.
Lemma eq_chunk_dec : forall (c1 c2 : chunk), { equal_chunks c1 c2 } + { ~ (equal_chunks c1 c2) } -> { cid c1 = cid c2 /\ in_aid c1 = in_aid c2 } + { ~ (cid c1 = cid c2 /\ in_aid c1 = in_aid c2) }.
    intros c1 c2 H. unfold equal_chunks in H.
    destruct H.
    - left. apply a.
    - right. apply n.
Defined.

Example check_equal_chunks : let c1 := new_chunk 1 2 in
                             let c2 := new_chunk 1 2 in equal_chunks c1 c2.
Proof.
    simpl. unfold equal_chunks. simpl. lia.
Qed.

Example uncheck_equal_chunks : let c1 := new_chunk 1 3 in
                               let c2 := new_chunk 2 3 in ~ equal_chunks c1 c2.
Proof.
    simpl. unfold equal_chunks. simpl. lia.
Qed.

(** the ordered set of chunks is an assembly *)
Record assembly (*(n : nat (*| n >= assemblyminsz /\ n <= assemblymaxsz*))*) : Type := mkassembly
    { nchunks : positive
    ; aid : positive
    ; valid : Prop
    ; apos : N
    ; encrypted : bool
    (* ; chunks : list chunk *)
    }.
(* Print mkassembly. *)
(** an experiment to have the size checked on creation.
    there is a problem that the size now needs to be set
    via a theorem. (see Theorem valid_assembly_size_20)      *)

(* Program Definition create_assembly (n : nat) (a : option assembly) (*_ : valid_assembly_size n*)
    : assembly :=
    (* : {a : assembly n | valid_assembly_size n} := *)
    let this_aid :=
        match a with
        | None => 0
        | Some a0 => S (aid a0) end
    in
    mkassembly n
               this_aid
               (valid_assembly_size n)
               0
               false
               (Vbuild (fun i (ip : i < n) => new_chunk i this_aid)).
Print create_assembly.

Fact valid_assembly_size_20 : valid_assembly_size 20.
Proof. unfold valid_assembly_size. unfold assemblyminsz. unfold assemblymaxsz. lia. Qed.
Lemma create_assembly_20 : let a := create_assembly None valid_assembly_size_20 in valid a.
Proof.
    unfold create_assembly. simpl. unfold valid_assembly_size.
    split.
    - unfold assemblyminsz. lia.
    - unfold assemblymaxsz. lia.
Qed.
Lemma create_two_assemblies_20 : let a1 := create_assembly None valid_assembly_size_20 in
                                 let a2 := create_assembly (Some a1) valid_assembly_size_20 in valid a2.
Proof.
    unfold create_assembly. simpl. unfold valid_assembly_size.
    split.
    - unfold assemblyminsz. lia.
    - unfold assemblymaxsz. lia.
Qed. *)

(** create list of chunks *)
(* Definition mk_chunk_list (n : positive) (p_aid : positive) : list chunk :=
  map (fun p_cid => new_chunk (Pos.of_nat p_cid) p_aid) (seq 1 (Pos.to_nat n)). *)

(* Compute mk_chunk_list 3 42. *)

(** create assembly (count of chunks) *)
Definition first_assembly (n : positive) : assembly :=
    let this_aid := 1
    in
    mkassembly n
               this_aid
               (valid_assembly_size n)
               N0
               false
               (*mk_chunk_list n this_aid*).
Definition new_assembly (a : assembly) : assembly :=
    let this_aid := 1 + (aid a) in
    let n := nchunks a in
    mkassembly n
               this_aid
               (valid_assembly_size n)
               N0
               false
               (*mk_chunk_list n this_aid*).

Lemma valid_assembly_20 : let a1 := first_assembly 20 in valid a1.
Proof.
    unfold first_assembly. simpl. unfold valid_assembly_size.
    split.
    - unfold assemblyminsz. lia.
    - unfold assemblymaxsz. lia.
Qed.
Lemma two_assemblies_20 : let a1 := first_assembly 20 in
                          let a2 := new_assembly a1 in valid a2
                                                       /\ 1 + aid a1 = aid a2.
                                                       (* /\ length (chunks a1) = 20%nat
                                                       /\ length (chunks a2) = 20%nat. *)
Proof.
    unfold new_assembly. simpl. unfold valid_assembly_size.
    split.
    + split.
      - unfold assemblyminsz. lia.
      - unfold assemblymaxsz. lia.
    + reflexivity.
Qed.


Definition add_data (len : N) (a : assembly) : assembly :=
    {| nchunks := nchunks a; aid := aid a; valid := valid a;
       apos := len + apos a; encrypted := encrypted a; (*chunks := chunks a*) |}.

End Data.

Section Access.
(** Reading from and writing to an assembly
    and the underlying chunks *)

(** NPair = (cid, index) *)
Definition ChIdx : Type := N * N.

Eval compute in N.modulo 23 16. (* cid *)
Eval compute in N.div 23 16. (* index *)
Definition apos_to_chidx (i : N) (n : positive) : ChIdx :=
  let n_N := N.of_nat (Pos.to_nat n) in
  let idx_N := (N.div i n_N) in
  ( N.modulo i n_N   (* the chunk id *)
  , idx_N ).         (* the index into the chunk *)
Eval compute in apos_to_chidx 23 16.
Eval compute in apos_to_chidx 3 16.
Eval compute in apos_to_chidx 233 16.
Eval compute in apos_to_chidx 0 16.
Eval compute in apos_to_chidx 1 16.
Eval compute in apos_to_chidx 2 16.
Eval compute in apos_to_chidx 10 16.

Definition chidx_to_apos (n : positive) (ch : ChIdx) : N :=
  let n_N := N.of_nat (Pos.to_nat n) in
  match ch with
  | (chno, chidx) => (chidx * n_N) + chno
  end.
    
Eval compute in chidx_to_apos 16 (1%N, 7%N).
Eval compute in chidx_to_apos 16 (10%N, 0%N).
Eval compute in chidx_to_apos 16 (apos_to_chidx 233 16) = 233%N. (* (9%N, 14%N) *)
Compute 233 mod 16.  (* 9%N *)
Compute N.div 233%N 16%N. (* 14%N *)
Compute N.mul 16%N (N.div 233%N 16%N). (* 224%N *)

Lemma compute_chidx_refl : forall (n : positive) (i : N),
    chidx_to_apos n (apos_to_chidx i n) = i.
Proof.
  unfold apos_to_chidx. unfold chidx_to_apos.
  intros. set j := N.of_nat (Pos.to_nat n).
  rewrite N.mul_comm.
  rewrite <- N.div_mod'.
  reflexivity.
Qed.
(** 1 subgoal (ID 21)
  
  n : positive
  i : N
  j := N.of_nat (Pos.to_nat n) : N
  ============================
  (i / j * j + i mod j)%N = i  *)
  (* the integral division is like "floor" so we need to add the remainder to the product to equal the original numerator again. *)
  (* is there such a theorem? *)


End Access.

Section Extraction.
(** Encryption and extraction of chunks to files *)


End Extraction.

Section Reconstitution.
(** Reconstituion of chunks from files and their decryption *)


End Reconstitution.

End Assembly.
