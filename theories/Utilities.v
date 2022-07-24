(**
      e L y K s e e R
*)

Module Export Utilities.

Require Import NArith Strings.String.

Section axioms.

(* returns random number
   (ignores argument)
*)
Axiom rnd : N -> N.

(* returns hash of random string:
   takes into account argument, and 
   random number, current time, hostname, and
   maybe other local information
*)
Axiom rnd256 : N -> string.

End axioms.

End Utilities.
