(**
      e L y K s e e R
*)

From Stdlib Require Import Strings.String Program.Basics Lia.
From Stdlib Require Import NArith PArith.
From Stdlib Require Import NArith.BinNat.
From Stdlib Require Import Logic.ProofIrrelevance.

From LXR Require Import Assembly Cstdio Nchunks Conversion.

Module CheckApos.

(**
 Module: CheckApos
 Description: verifying properties of assembly indexing (apos) calculations.
 *)

Set Implicit Arguments.
Unset Strict Implicit.

Open Scope positive_scope.
Open Scope N_scope.

Section Non_Local_Assembly.

(* a non-local apos calculation that fits in 32 bits *)
(* features: 
     - non-local by separating by n_chunks bytes
     - not touching first (n_chunks - tag_len) bytes (reserved for random data), and last tag_len bytes (GCM tag)
     - index calculation fits in 32 bit *)
Local Definition idx2apos32 (idx : N) (a : assemblyinformation) : N :=
    let nch : N := Nchunks.to_N (nchunks a) in
    let m : N := Assembly.chunksize_N - 1 in
    nch * (idx mod m) + (N.div idx m) + (nch - Cstdio.tag_len).

(* a non-local apos calculation that requires 64 bits *)
(* features: 
     - non-local by separating by (chunkwidth * chunkheight - 1) bytes
     - not touching first (n_chunks - tag_len) bytes (reserved for random data), and last tag_len bytes (GCM tag)
     - index calculation requires 64 bit *)
Local Definition idx2apos64 (idx : N) (a : assemblyinformation) : N :=
     let nch : N := Nchunks.to_N (nchunks a) in
     let asize : N := Assembly.assemblysize (nchunks a) in
     let eff_asize : N := asize - nch in
     let proj : N := idx * (Assembly.chunksize_N - 1) in
     (proj mod eff_asize) + (N.div proj eff_asize) + nch - Cstdio.tag_len.

Example apos32_of_index_0 :
    let ai := mkassembly (Nchunks.from_positive 17) "none" 0 in
    idx2apos32 0 ai = 1.
Proof.
    intros. eauto.
Qed.
Example apos64_of_index_0 :
    let ai := mkassembly (Nchunks.from_positive 17) "none" 0 in
    idx2apos64 0 ai = 1.
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
    idx2apos64 1 ai - idx2apos64 0 ai = Assembly.chunksize_N - 1.
Proof.
    intros. eauto.
Qed.

Section HelperLemmas.

(* Helper lemma for sigma type equality *)
Lemma exist_ext : forall (A : Type) (P : A -> Prop) (x y : A) (px : P x) (py : P y),
  x = y ->
  exist P x px = exist P y py.
Proof.
  intros. subst. f_equal. apply proof_irrelevance.
Qed.

(* Nchunks bounds - any positive that turns into a Nchunk.t *)
Lemma nchunks_ge_tag_len : forall (n : positive),
  Cstdio.tag_len <= Nchunks.to_N (Nchunks.from_positive n).
Proof.
    intros n.
    unfold Cstdio.tag_len.
    assert (H: (16 <= Nchunks.to_positive (Nchunks.from_positive n))%positive).
    { unfold Nchunks.from_positive, Nchunks.to_positive, Nchunks.min_n, Nchunks.max_n.
      simpl. apply Pos.min_glb; [discriminate | apply Pos.le_max_r]. }
    unfold Nchunks.to_N, Conversion.pos2N, Pos.le in H. exact H.
Qed.

Lemma chunksize_m1_pos : 0 < Assembly.chunksize_N - 1.
Proof.
  unfold Assembly.chunksize_N, Assembly.chunksize, Assembly.chunkwidth, Assembly.chunklength.
  simpl. reflexivity.
Qed.

Lemma eff_asize_factored : forall nch,
  0 < nch ->
  let asize := Assembly.chunksize_N * nch in
  let eff_asize := asize - nch in
  eff_asize = nch * (Assembly.chunksize_N - 1).
Proof.
  intros. unfold eff_asize. unfold asize.
  rewrite N.mul_sub_distr_l.
  rewrite N.mul_1_r. lia.
Qed.

(* Invariant: all Nchunks.t values are idempotent under from_positive ∘ to_positive *)
Lemma nchunks_idempotent : forall (nc : Nchunks.t),
  Nchunks.from_positive (Nchunks.to_positive nc) = nc.
Proof.
  intros nc.
  unfold Nchunks.from_positive, Nchunks.to_positive.
  destruct nc as [p [Hmin Hmax]].
  simpl.
  apply exist_ext.
  unfold Nchunks.min_n, Nchunks.max_n in *.
  (* Have: N.pos 16 <= N.pos p <= N.pos 256 *)
  (* Need: Pos.min 256 (Pos.max p 16) = p *)
  assert (Hmin_pos: (16 <= p)%positive) by (unfold Pos.le; simpl in Hmin; exact Hmin).
  assert (Hmax_pos: (p <= 256)%positive) by (unfold Pos.le; simpl in Hmax; exact Hmax).
  rewrite Pos.max_l by assumption.
  rewrite Pos.min_r by assumption.
  reflexivity.
Qed.

Corollary nchunks_t_ge_tag_len : forall (nc : Nchunks.t),
  Cstdio.tag_len <= Nchunks.to_N nc.
Proof.
  intros nc.
  rewrite <- (nchunks_idempotent nc).
  apply nchunks_ge_tag_len.
Qed.

End HelperLemmas.

Section MainTheorems.

(** Closed form equals naive form *)
Theorem idx2apos32_closed_form_equiv : forall idx (a : assemblyinformation),
  let nch : N := Nchunks.to_N (nchunks a) in
  let asize : N := Assembly.assemblysize (nchunks a) in
  let eff_asize : N := asize - nch in
  let m : N := Assembly.chunksize_N - 1 in
  let naive := (idx * nch) mod eff_asize + (idx * nch) / eff_asize + (nch - Cstdio.tag_len) in
  let closed := nch * (idx mod m) + (idx / m) + (nch - Cstdio.tag_len) in
  0 < nch ->
  naive = closed.
Proof.
  intros. unfold naive, closed, eff_asize, asize, Assembly.assemblysize.
  fold nch.
  (* Rewrite eff_asize as nch * m *)
  assert (Heff: Assembly.chunksize_N * nch - nch = nch * m).
  { unfold m. rewrite N.mul_sub_distr_l. rewrite N.mul_1_r. lia. }
  rewrite Heff.
  rewrite (N.mul_comm nch m). 
  rewrite N.mul_mod_distr_r.
  2: { (* subgoal m <> 0 *)
    unfold m.
    assert (0 < Assembly.chunksize_N - 1) by apply chunksize_m1_pos.
    lia.
  }
  2: { (* subgoal nch <> 0 *)
    lia.
  }
  apply N.add_cancel_r.
  rewrite (N.mul_comm (idx mod m) nch).
  rewrite N.div_mul_cancel_r.
  3: { lia. } (* subgoal nch <> 0 *)
  2: { (* subgoal m <> 0 *)
    unfold m. 
    assert (0 < Assembly.chunksize_N - 1) by apply chunksize_m1_pos.
    lia.
  }
  reflexivity.
  (* Alternative: use N.div_mul_cancel_l and N.mod_mul_r if available *)
Qed.

(** Injectivity *)
Theorem idx2apos32_injective : forall idx1 idx2 (a : assemblyinformation),
  let nch : N := Nchunks.to_N (nchunks a) in
  let eff_asize : N := Assembly.assemblysize (nchunks a) - nch in
  0 < nch ->
  idx1 < eff_asize ->
  idx2 < eff_asize ->
  idx2apos32 idx1 a = idx2apos32 idx2 a ->
  idx1 = idx2.
Proof.
  intros. unfold idx2apos32 in H2.
  fold nch in H2.
  apply N.add_cancel_r in H2.
  (* quotient *)
  assert (Hq: idx1 / (chunksize_N - 1) = idx2 / (chunksize_N - 1)).
  {
    unfold eff_asize in H0, H1.
    assert (Hq1: idx1 / (chunksize_N - 1) < nch).
    { apply N.div_lt_upper_bound.
        - pose proof chunksize_m1_pos. lia.
        - rewrite N.mul_comm.
          assert (assemblysize (nchunks a) - nch = nch * (chunksize_N - 1)).
          { unfold assemblysize.
            rewrite <- eff_asize_factored by exact H.
            reflexivity.
          }
          rewrite <- H3.
          exact H0.
    }
    assert (Hq2: idx2 / (chunksize_N - 1) < nch).
    { (* Same proof as Hq1 but for idx2 *)
        apply N.div_lt_upper_bound.
        - pose proof chunksize_m1_pos. lia.
        - unfold assemblysize in H1. fold nch in H1.
        nia.
    }
    assert (Hr1: idx1 mod (chunksize_N - 1) < chunksize_N - 1).
    { apply N.mod_lt. pose proof chunksize_m1_pos. lia. }
    assert (Hr2: idx2 mod (chunksize_N - 1) < chunksize_N - 1).
    { apply N.mod_lt. pose proof chunksize_m1_pos. lia. }
    destruct (N.lt_trichotomy (idx1 mod (chunksize_N - 1)) 
                              (idx2 mod (chunksize_N - 1))) 
      as [Hlt | [Heq | Hgt]].
    - (* case r1 < r2: contradiction *)
      assert (H3: idx2 mod (chunksize_N - 1) >= 
                  idx1 mod (chunksize_N - 1) + 1) by lia.
      assert (H4: nch * (idx1 mod (chunksize_N - 1) + 1) <= 
                  nch * (idx2 mod (chunksize_N - 1))).
      { apply N.mul_le_mono_l. lia. }
  
      (* Expand multiplication *)
      rewrite N.mul_add_distr_l in H4.
      rewrite N.mul_1_r in H4.

      assert (H5: nch * (idx1 mod (chunksize_N - 1)) + nch + 
                    idx2 / (chunksize_N - 1) <= 
                    nch * (idx2 mod (chunksize_N - 1)) + 
                    idx2 / (chunksize_N - 1)).
      { lia. }
      replace (nch * (idx2 mod (chunksize_N - 1)) + 
                idx2 / (chunksize_N - 1))
      with (nch * (idx1 mod (chunksize_N - 1)) + 
                idx1 / (chunksize_N - 1))
      in H5.
      exfalso.  (* derive contradiction *)
      assert (nch + idx2 / (chunksize_N - 1) <= 
        idx1 / (chunksize_N - 1)) by lia.
      (* Derive nch < nch, absurd *)
      assert (nch <= idx1 / (chunksize_N - 1)) as Hcontra.
      { (* nch <= nch + q2 <= q1 *)
        eapply N.le_trans.
        - apply N.le_add_r.
        - exact H6.
      }
      (* Now: nch <= q1 AND q1 < nch impossible *)
      pose proof (N.lt_le_incl _ _ Hq1) as Hq1'.
      (* Hq1': q1 <= nch *)
      apply (N.lt_irrefl nch).
      eapply N.le_lt_trans; eassumption.
    - (* Case r1 = r2: conclude q1 = q2 *)
      rewrite Heq in H2.
      (* Now H2: nch * r + q1 = nch * r + q2 *)
      lia.
    - (* case r1 > r2: symmetric contradiction *)
      assert (H3: idx1 mod (chunksize_N - 1) >= 
                  idx2 mod (chunksize_N - 1) + 1) by lia.
      assert (H4: nch * (idx2 mod (chunksize_N - 1) + 1) <= 
                  nch * (idx1 mod (chunksize_N - 1))).
      { apply N.mul_le_mono_l. lia. }

      (* Expand multiplication *)
      rewrite N.mul_add_distr_l in H4.
      rewrite N.mul_1_r in H4.

      assert (H5: nch * (idx2 mod (chunksize_N - 1)) + nch + 
                  idx1 / (chunksize_N - 1) <= 
                  nch * (idx1 mod (chunksize_N - 1)) + 
                  idx1 / (chunksize_N - 1)).
      { lia. }
    
      symmetry in H2.  (* Swap sides *)
      replace (nch * (idx1 mod (chunksize_N - 1)) + 
              idx1 / (chunksize_N - 1))
          with (nch * (idx2 mod (chunksize_N - 1)) + 
              idx2 / (chunksize_N - 1))
          in H5.
    
      exfalso.
      assert (nch + idx1 / (chunksize_N - 1) <= 
              idx2 / (chunksize_N - 1)) by lia.
    
      assert (nch <= idx2 / (chunksize_N - 1)) as Hcontra.
      { eapply N.le_trans.
          - apply N.le_add_r.
          - exact H6.
    }
    
    pose proof (N.lt_le_incl _ _ Hq2) as Hq2'.
    apply (N.lt_irrefl nch).
    eapply N.le_lt_trans; eassumption.
    
  }
  (* remainder *)
  assert (Hr: idx1 mod (chunksize_N - 1) = idx2 mod (chunksize_N - 1)).
  {
    rewrite Hq in H2. nia.
  }
  rewrite (N.div_mod idx1 (chunksize_N - 1)) at 1.
  2: { unfold chunksize_N, chunksize, chunkwidth, chunklength. compute. discriminate. }
  rewrite <- (N.div_mod idx1 (chunksize_N - 1)).
  2: { pose proof chunksize_m1_pos. lia. }
  (* final *)
  rewrite (N.div_mod idx1 (chunksize_N - 1)).
  2: { pose proof chunksize_m1_pos. lia. }
  rewrite (N.div_mod idx2 (chunksize_N - 1)).
  2: { pose proof chunksize_m1_pos. lia. }
  rewrite Hq, Hr.
  reflexivity.
Qed.

(** Range: lower bound *)
Theorem idx2apos32_lower_bound : forall idx (a : assemblyinformation),
  let nch : N := Nchunks.to_N (nchunks a) in
  nch - Cstdio.tag_len <= idx2apos32 idx a.
Proof.
  intros. unfold idx2apos32.
  rewrite N.add_comm.
  apply N.le_add_r.
Qed.

(** Range: upper bound *)
Theorem idx2apos32_upper_bound : forall idx (a : assemblyinformation),
  let nch : N := Nchunks.to_N (nchunks a) in
  let asize : N := Assembly.assemblysize (nchunks a) in
  let eff_asize : N := asize - nch in
  0 < nch ->
  idx < eff_asize ->
  idx2apos32 idx a < asize - Cstdio.tag_len.
Proof.
  intros. unfold idx2apos32, asize, eff_asize, Assembly.assemblysize.
  fold nch.
  (* Get bounds from H0 *)
  assert (Hmod: idx mod (chunksize_N - 1) < chunksize_N - 1).
  { apply N.mod_lt. pose proof chunksize_m1_pos. lia. }
  assert (Hdiv: idx / (chunksize_N - 1) < nch).
  { apply N.div_lt_upper_bound.
    - pose proof chunksize_m1_pos. lia.
    - unfold eff_asize in H0. unfold asize, assemblysize in H0.
      fold nch in H0.
      nia. (* Derives (C-1)*n from C*n - n *)
  }
  (* From strict bounds: r < C-1 and q < nch *)
  (* We have: r <= C-2 and q <= nch-1 *)
  (* So: nch*r + q <= nch*(C-2) + (nch-1) = nch*C - nch - 1 *)
  (* Adding offset: ... + (nch - 16) = nch*C - 16 - 1 < nch*C - 16 *)
  
  assert (Hstrict: nch * (idx mod (chunksize_N - 1)) + 
                   idx / (chunksize_N - 1) < 
                   nch * chunksize_N - nch).
  { (* Use that r < C-1 means r <= C-2, q < nch means q <= nch-1 *)
    assert (idx mod (chunksize_N - 1) <= chunksize_N - 2) by lia.
    assert (idx / (chunksize_N - 1) <= nch - 1) by lia.
    nia.
  }
  
  (* From Hstrict + offset *)
  assert (nch * (idx mod (chunksize_N - 1)) + 
          idx / (chunksize_N - 1) + (nch - tag_len) <
          nch * chunksize_N - tag_len).
  { (* From Hstrict: x < nch*C - nch *)
    (* Want: x + (nch - tag) < nch*C - tag *)
    (* Rearrange: x + nch < nch*C, then subtract tag from goal *)
  
    assert (H_tmp: nch * (idx mod (chunksize_N - 1)) + 
                   idx / (chunksize_N - 1) + nch < 
                   nch * chunksize_N).
    { (* From Hstrict: x < nch*C - nch means x + nch < nch*C *)
      apply N.add_lt_mono_r with (p := nch) in Hstrict.
      rewrite N.sub_add in Hstrict.
      - exact Hstrict.
      - pose proof chunksize_m1_pos. nia.
    }
    - (* From H_tmp: x + nch < nch*C, derive x + (nch - tag) < nch*C - tag *)
      assert (Hge: tag_len <= nch).
      { (* All Nchunks.t values have to_N >= min_n = 16 = tag_len *)
        unfold nch.
        apply nchunks_t_ge_tag_len.
      }
      assert (Hge2: tag_len <= nch * chunksize_N).
      { pose proof chunksize_m1_pos. nia. }
      (* Add tag_len to both sides then use N.sub_add to cancel *)
      apply N.add_lt_mono_r with (p := tag_len).
      rewrite <- N.add_assoc. (* (x + (nch - tag)) + tag = x + ((nch - tag) + tag) *)
      rewrite N.sub_add by lia. (* (nch - tag) + tag = nch *)
      rewrite N.sub_add by lia. (* (nch*C - tag) + tag = nch*C *)
      exact H_tmp.
  }
  (* Use established bound to finish *)
  rewrite (N.mul_comm chunksize_N nch).
  exact H1. 
  

Qed.

(** Maximum value of idx2apos32 when nch = 128 *)
Definition idx2apos32_max_128 : N := 128 * 262142 + 127 + 112.

(** Verify max is under 2^32 by computation *)
Example max_128_fits_32bit : idx2apos32_max_128 < 2^32.
Proof. unfold idx2apos32_max_128. reflexivity. Qed.

(** 32-bit safety for nch <= 128 *)
Theorem idx2apos32_fits_32bit : forall idx (a : assemblyinformation),
  let nch : N := Nchunks.to_N (nchunks a) in
  let eff_asize : N := Assembly.assemblysize (nchunks a) - nch in
  nch <= 128 ->
  idx < eff_asize ->
  idx2apos32 idx a < 2^32.
Proof.
  intros idx a nch eff_asize Hnch Hidx.
  unfold idx2apos32. fold nch.

  (* Establish concrete bound *)
  assert (Hbound: nch * (idx mod (Assembly.chunksize_N - 1)) +
                  idx / (Assembly.chunksize_N - 1) + (nch - Cstdio.tag_len)
                  <= idx2apos32_max_128).
  {
    unfold idx2apos32_max_128.
    (* Need to show each component bounded *)
    assert (Hm: Assembly.chunksize_N - 1 = 262143).
    { unfold Assembly.chunksize_N, Assembly.chunksize, Assembly.chunkwidth, Assembly.chunklength.
      reflexivity. }

    assert (Hmod: idx mod (Assembly.chunksize_N - 1) <= 262142).
    { assert (idx mod (Assembly.chunksize_N - 1) < Assembly.chunksize_N - 1).
      { apply N.mod_lt. unfold Assembly.chunksize_N, Assembly.chunksize, Assembly.chunkwidth, Assembly.chunklength.
        compute. discriminate. }
      rewrite Hm in H.
      (* 262143 = N.succ 262142, so x < 262143 iff x <= 262142 *)
      assert (Hsucc: 262143 = N.succ 262142) by reflexivity.
      rewrite Hsucc in H.
      apply N.lt_succ_r. exact H. }

    assert (Hdiv: idx / (Assembly.chunksize_N - 1) <= 127).
    { unfold eff_asize, Assembly.assemblysize in Hidx. fold nch in Hidx.
      assert (Assembly.chunksize_N * nch - nch = nch * 262143).
      { rewrite <- Hm. rewrite N.mul_comm. rewrite N.mul_sub_distr_l.
        rewrite N.mul_1_r. reflexivity. }
      rewrite H in Hidx.
      (* idx < nch * 262143 <= 128 * 262143, so idx / 262143 < 128 *)
      assert (Hlt128: idx / (Assembly.chunksize_N - 1) < 128).
      { rewrite Hm.
        apply N.div_lt_upper_bound.
        - compute. discriminate.
        - rewrite N.mul_comm.
          eapply N.lt_le_trans.
          + exact Hidx.
          + apply N.mul_le_mono_r. exact Hnch. }
      (* x < 128 means x <= 127 *)
      assert (Hsucc128: 128 = N.succ 127) by reflexivity.
      rewrite Hsucc128 in Hlt128.
      apply N.lt_succ_r. exact Hlt128. }

    assert (Hoffset: nch - Cstdio.tag_len <= 112).
    { unfold Cstdio.tag_len. lia. }

    (* Combine bounds *)
    assert (nch * (idx mod (Assembly.chunksize_N - 1)) <= 128 * 262142).
    { apply N.mul_le_mono; assumption. }

    lia.
  }

  (* Use computed verification *)
  eapply N.le_lt_trans.
  - exact Hbound.
  - exact max_128_fits_32bit.
Qed.

End MainTheorems.

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
    let asize : N := Assembly.chunksize_N * nch in
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
    let asize : N := Assembly.chunksize_N * nch in
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
    let asize : N := Assembly.chunksize_N * nch in
    let eff_asize : N := asize - nch in
    let proj : N := (eff_asize - 1) * (Assembly.chunksize_N - 1) in
    bits_of proj = 43.
Proof.
    intros. unfold bits_of.
    simpl. reflexivity.
Qed.
Example nchunks_256_max_bits_of_idx2apos64_eq_44 :
    let nch : N := 256 in
    let asize : N := Assembly.chunksize_N * nch in
    let eff_asize : N := asize - nch in
    let proj : N := (eff_asize - 1) * (Assembly.chunksize_N - 1) in
    bits_of proj = 44.
Proof.
    intros. unfold bits_of.
    simpl. reflexivity.
Qed.

End Non_Local_Assembly.

End CheckApos.
