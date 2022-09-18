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

From LXR Require Import Buffer Configuration Conversion Utilities.

(** constants *)
Section Constants.

(** chunk 2D size *)
Definition chunkwidth  : positive := 256%positive.
Definition chunklength : positive := 1024%positive.
Definition chunkwidth_N  : N := 256%N.
Definition chunklength_N : N := 1024%N.
Definition chunksize_N : N := 256%N * 1024%N.

(** assembly size range (count of chunks) *)
Definition assemblyminsz : positive := 16%positive.
Definition assemblymaxsz : positive := 256%positive.
Definition assemblyminsz_N : N := 16%N.
Definition assemblymaxsz_N : N := 256%N.

End Constants.

Section Data.

(** data structures *)

(* Definition valid_assembly_size (p : positive) : Prop :=
    p >= assemblyminsz /\ p <= assemblymaxsz.

Definition valid_assembly_size_N (n : N) : Prop :=
    n >= assemblyminsz_N /\ n <= assemblymaxsz_N.

Lemma invalid_assembly_low : forall n, n < assemblyminsz_N /\ valid_assembly_size n -> False.
Proof.
    intros n. unfold valid_assembly_size.
    unfold assemblyminsz_N. unfold assemblymaxsz_N.
    lia.
Qed.
Lemma invalid_assembly_hi : forall n, n > assemblymaxsz_N /\ valid_assembly_size n -> False.
Proof.
    intros n. unfold valid_assembly_size.
    unfold assemblyminsz_N. unfold assemblymaxsz_N.
    lia.
Qed. *)

(** encrypted data can be extracted as chunks 
    a chunk is unique per assembly
*)
Record chunk : Type := mkchunk
    { cid : positive
    ; in_anum : positive
    ; buffer : Buffer.buffer
    }.
(* Print chunk. *)
Definition new_chunk (p_cid p_anum : positive) : chunk :=
    mkchunk p_cid p_anum (new_buffer chunksize_N).

Definition equal_chunks (c1 c2 : chunk) : Prop :=
    cid c1 = cid c2 /\ in_anum c1 = in_anum c2.
Lemma eq_chunk_dec : forall (c1 c2 : chunk), { equal_chunks c1 c2 } + { ~ (equal_chunks c1 c2) } -> { cid c1 = cid c2 /\ in_anum c1 = in_anum c2 } + { ~ (cid c1 = cid c2 /\ in_anum c1 = in_anum c2) }.
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
    ; anum : positive
    ; aid : string
    (* ; valid : Prop *)
    ; apos : N
    ; encrypted : bool
    ; chunks : list chunk
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
Fixpoint seq_p (start : positive) (len : nat) {struct len} : list positive :=
    match len with
    | 0%nat => nil
    | S len' => start :: seq_p (Pos.succ start) len'
    end.

Definition mk_chunk_list (n : positive) (p_aid : positive) : list chunk :=
  map (fun p_cid => new_chunk p_cid p_aid) (seq_p 1 (Pos.to_nat n)).

(* Compute mk_chunk_list 3 42. *)

(** create assembly (count of chunks) *)
Definition first_assembly (my_id : N) (n : positive) : assembly :=
    let this_anum := 1%positive
    in
    mkassembly n
               this_anum
               (Utilities.rnd256 my_id)
               (* (valid_assembly_size n) *)
               N0
               false
               (mk_chunk_list n this_anum).
Definition new_assembly (my_id : N) (a : assembly) : assembly :=
    let anum0 := anum a in
    let this_anum := Pos.succ anum0 in
    let n : positive := nchunks a in
    mkassembly n
               this_anum
               (Utilities.rnd256 my_id)
               (* (valid_assembly_size n) *)
               N0
               false
               (mk_chunk_list n this_anum).

(* Lemma valid_assembly_20 : let a1 := first_assembly 42%N 20 in valid a1.
Proof.
    unfold first_assembly. simpl. unfold valid_assembly_size.
    split.
    - unfold assemblyminsz. lia.
    - unfold assemblymaxsz. lia.
Qed.
Lemma two_assemblies_20 : let a1 := first_assembly 42%N 20 in
                          let a2 := new_assembly 42%N a1 in valid a2
                                                       /\ 1 + anum a1 = anum a2.
                                                       (* /\ length (chunks a1) = 20%nat
                                                       /\ length (chunks a2) = 20%nat. *)
Proof.
    unfold new_assembly. simpl. unfold valid_assembly_size.
    split.
    + split.
      - unfold assemblyminsz. lia.
      - unfold assemblymaxsz. lia.
    + reflexivity.
Qed. *)


Definition add_data (buf : Buffer.buffer) (a : assembly) : assembly :=
    (* let len := Conversion.pos2N(width buf * height buf) in *)
    let len := len buf in
    {| nchunks := nchunks a; anum := anum a; aid := aid a; (* valid := valid a; *)
       apos := len + apos a; encrypted := encrypted a; chunks := chunks a |}.

End Data.

Section Access.
(** Reading from and writing to an assembly
    and the underlying chunks *)

(** NPair = (cid, index) *)
Definition ChIdx : Type := N * N.

Eval compute in N.modulo 2 16. (* cid *)
Eval compute in N.div 23 16. (* index *)
Definition apos_to_chidx (n : positive) (i : N) : ChIdx :=
  let n_N := pos2N n in
  ( N.modulo i n_N   (* the chunk id (0 .. n-1) *)
  , N.div i n_N ).   (* the index into the chunk (0 .. l-1) *)
Eval compute in apos_to_chidx 16 23.
Eval compute in apos_to_chidx 16 3.
Eval compute in apos_to_chidx 16 233.
Eval compute in apos_to_chidx 16 0.
Eval compute in apos_to_chidx 16 1.
Eval compute in apos_to_chidx 16 2.
Eval compute in apos_to_chidx 16 17.

Definition chidx_to_apos (n : positive) (ch : ChIdx) : N :=
  let n_N := pos2N n in
  match ch with
  | (chno, chidx) => (chidx * n_N) + chno
  end.
    
Eval compute in chidx_to_apos 2 (1%N, 7%N).
Eval compute in chidx_to_apos 16 (1%N, 1%N).
Eval compute in chidx_to_apos 16 (1%N, 7%N).
Eval compute in chidx_to_apos 16 (10%N, 0%N).
Eval compute in chidx_to_apos 16 (apos_to_chidx 16 233) = 233%N. (* (9%N, 14%N) *)

Lemma compute_chidx_refl : forall (n : positive) (i : N),
    chidx_to_apos n (apos_to_chidx n i) = i.
Proof.
  unfold apos_to_chidx. unfold chidx_to_apos.
  intros. set j := pos2N n.
  rewrite N.mul_comm.
  rewrite <- N.div_mod'.
  reflexivity.
Qed.
(** 1 subgoal (ID 21)
  
  n : positive
  i : N
  j := pos2N n : N
  ============================
  i / j * j + i mod j = i  *)
  (* the integral division is like "floor" so we need to add the remainder to the product to equal the original numerator again. *)
  (* is there such a theorem? *)


End Access.

Section Extraction.
(** Encryption and extraction of chunks to files *)

Axiom chunk_identifier : configuration -> string -> positive -> string.

Axiom store_buffer_to_path : Buffer.buffer_t -> configuration -> string -> bool.

Definition extract_chunk (config : configuration) (aid : string) (c : chunk) : bool :=
    let cpath := chunk_identifier config aid (cid c) in
    store_buffer_to_path (buf (buffer c)) config cpath.

Fixpoint extract_chunks (config : configuration) (aid : string) (cs : list chunk) {struct cs} : list bool :=
    match cs with
    | nil => nil
    | c :: rcs =>
        (extract_chunk config aid c) :: extract_chunks config aid rcs
    end.

Definition extract (config : configuration) (a : assembly) : bool :=
    let res := extract_chunks config (aid a) (chunks a) in
    N.of_nat(List.length (filter (fun r => r) res)) =? pos2N(nchunks a).

End Extraction.

Section Reconstitution.
(** Reconstituion of chunks from files and their decryption *)

Axiom load_buffer_from_path : configuration -> string -> option Buffer.buffer_t.

Definition restore_chunk (config : configuration) (aid : string) (c : chunk) : option Buffer.buffer_t :=
    let cpath := chunk_identifier config aid (cid c) in
    load_buffer_from_path config cpath.

Definition set_buffer (c : chunk) (b : Buffer.buffer_t) : chunk :=
    {| cid := cid c; in_anum := in_anum c; buffer := init_buffer b |}.

Fixpoint restore_chunks (config : configuration) (a : assembly) (cs : list chunk) {struct cs} : list chunk :=
    match cs with
    | nil => nil
    | c :: rcs =>
            match (restore_chunk config (aid a) c) with
            | None => restore_chunks config a rcs
            | Some b => (set_buffer c b) :: (restore_chunks config a rcs)
            end
    end.
(* Print restore_chunks. *)

Definition restore (config : configuration) (a : assembly) : assembly :=
    let chunks' := restore_chunks config a (chunks a) in
    {| nchunks := nchunks a; anum := anum a; aid := aid a; (* valid := valid a; *)
       apos := 0; encrypted := false; chunks := chunks' |}.


End Reconstitution.

End Assembly.
