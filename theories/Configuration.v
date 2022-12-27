(**
      e L y K s e e R
*)

Module Export Configuration.

Require Import NArith PArith.
From Coq Require Import Strings.String.

From LXR Require Import Nchunks.

Record configuration : Type :=
    mkconfiguration
        { config_nchunks : Nchunks.t
        ; path_chunks : string
        ; path_db : string
        ; my_id : N
        }.

End Configuration.