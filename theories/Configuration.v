(**
      e L y K s e e R
*)

Module Export Configuration.

Require Import PArith.
From Coq Require Import Strings.String.

Record configuration : Type :=
    mkconfiguration
        { num_chunks : positive
        ; path_chunks : string
        ; path_meta : string
        }.

End Configuration.