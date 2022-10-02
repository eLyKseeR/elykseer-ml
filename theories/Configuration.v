(**
      e L y K s e e R
*)

Module Export Configuration.

Require Import NArith PArith.
From Coq Require Import Strings.String.

Record configuration : Type :=
    mkconfiguration
        { path_chunks : string
        ; path_meta : string
        ; my_id : N
        }.

End Configuration.