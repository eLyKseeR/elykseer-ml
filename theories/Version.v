(**
      e L y K s e e R
*)

Module Export Version.

Require Import Strings.String.
Open Scope string_scope.

Definition major : string := "0".
Definition minor : string :=     "9".
Definition build : string :=         "1".
Definition version : string := major ++ "." ++ minor ++ "." ++ build.

End Version.
