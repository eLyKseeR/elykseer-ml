(**
      e L y K s e e R
*)

Require Import Strings.String.

Module Export Version.

Open Scope string_scope.

Definition major : string := "0".
Definition minor : string :=     "9".
Definition build : string :=         "7".
Definition version : string := major ++ "." ++ minor ++ "." ++ build.

End Version.
