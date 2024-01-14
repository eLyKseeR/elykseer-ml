(**
      e L y K s e e R
*)

Require Import PArith Lists.List Strings.String.

Module Export Utilities.

Section make_list.

Definition make_list (n : positive) : list positive :=
   let natlist := List.seq 1 (Pos.to_nat n) in
   List.map (fun x => Pos.of_nat x) natlist.

End make_list.

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

End axioms.

End Utilities.
