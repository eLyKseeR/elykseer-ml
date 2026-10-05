(**
      e L y K s e e R
*)

From Stdlib Require Import Strings.String.

Module Export Version.

Open Scope string_scope.

Definition major : string := "0".
Definition minor : string :=     "10".
Definition build : string :=         "1".
Definition version : string := major ++ "." ++ minor ++ "." ++ build.

End Version.
