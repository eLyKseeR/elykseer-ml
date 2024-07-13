(**
      e L y K s e e R
*)

Require Import PArith Lists.List Strings.String.

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
   takes into account argument, and 
   random number, current time, hostname, and
   maybe other local information
*)
Axiom rnd256 : string -> string.

(* returns hash of a string
*)
Axiom sha3_256 : string -> string.

End axioms.

End Utilities.
