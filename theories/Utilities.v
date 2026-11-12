(**
      e L y K s e e R
*)

From Stdlib Require Import PArith NArith Lists.List Strings.String Lia.

Set Implicit Arguments.

Import ListNotations.

Module Export Utilities.

Section make_list.

Definition make_list (n : positive) : list positive :=
   let natlist := List.seq 1 (Pos.to_nat n) in
   List.map (fun x => Pos.of_nat x) natlist.

End make_list.

Section list_manipulation.

Variable A : Set.

Fixpoint drop_list [A : Set] (n : nat) (ls : list A) {struct ls} : list A :=
   match n, ls with
   | O, _ => ls
   | S k, nil => nil
   | S k, _ :: r => drop_list k r
   end.
(*
Print drop_list.
Print Implicit drop_list.

Compute drop_list 1 [0;1;2].  (* = [1;2] *)
*)

Example drop_zero_from_list : forall {ls : list nat},
   drop_list 0 ls = ls.
Proof.
   intros. induction ls.
   - reflexivity.
   - unfold drop_list. reflexivity.
Qed.

Fixpoint take_list [A : Set] (n : nat) (ls : list A) {struct ls} : list A :=
   match n, ls with
   | O, _ => []
   | S k, nil => []
   | S k, a :: r => a :: take_list k r
   end.

(* Compute take_list 2 [0;1;2;3].
Compute take_list 0 [0;1;2;3]. *)

Example take_zero_from_list : forall {ls : list nat},
   take_list 0 ls = [].
Proof.
   intros. induction ls.
   - reflexivity.
   - unfold take_list. reflexivity.
Qed.

Lemma take_list_length : forall [B : Set] n (ls : list B),
   List.length (take_list n ls) = Nat.min n (List.length ls).
Proof. intros B n ls; revert n; induction ls; destruct n; simpl; auto. Qed.

Lemma drop_list_length : forall [B : Set] n (ls : list B),
   List.length (drop_list n ls) = List.length ls - n.
Proof. intros B n ls; revert n; induction ls; destruct n; simpl; auto. Qed.

Lemma take_list_in : forall [B : Set] n (ls : list B) x,
   In x (take_list n ls) -> In x ls.
Proof.
   intros B n ls; revert n; induction ls; destruct n; simpl; intros x H; try contradiction.
   destruct H; [left | right; eapply IHls]; eauto.
Qed.

Lemma drop_list_in : forall [B : Set] n (ls : list B) x,
   In x (drop_list n ls) -> In x ls.
Proof.
   intros B n ls; revert n; induction ls; destruct n; simpl; intros x H; auto.
   right; eapply IHls; eauto.
Qed.

(* taking a+b elements is taking a, then b of the rest *)
Lemma take_list_add : forall [B : Set] n m (ls : list B),
   take_list (n + m) ls = take_list n ls ++ take_list m (drop_list n ls).
Proof.
   intros B n m ls; revert n; induction ls as [|x r IH]; destruct n; simpl; auto.
   - destruct m; auto.
   - f_equal; auto.
Qed.

Lemma take_list_app_ge : forall [B : Set] n (l1 l2 : list B),
   List.length l1 <= n -> take_list n (l1 ++ l2) = l1 ++ take_list (n - List.length l1) l2.
Proof.
   intros B n l1; revert n; induction l1 as [|x r IHl1]; intros n l2 H; simpl.
   - rewrite PeanoNat.Nat.sub_0_r; reflexivity.
   - destruct n; simpl in *; [lia|]. f_equal; apply IHl1; lia.
Qed.

End list_manipulation.

Section axioms.

(* initialise the random number generator
*)
(* Axiom rndsetup : N -> N. *)

(* returns random number
   (ignores argument)
*)
Axiom rnd : N -> N.

(* returns hash of random string:
   takes into account the argument and 256 random bits
*)
Axiom rnd256 : string -> string.

(* returns hash of a string
*)
Axiom sha3_256 : string -> string.

(* wall clock in milliseconds *)
Axiom now_ms : unit -> N.

(* sleeps until the clock reaches the argument (returns at once if it already has);
   returns the clock after waking up, so that extraction keeps the call *)
Axiom sleep_until : N -> N.

End axioms.

End Utilities.
