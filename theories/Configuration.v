(**
      e L y K s e e R
*)

Require Import NArith PArith.
From Coq Require Import Strings.String.

From LXR Require Import Nchunks.
From LXR Require Import Tracer.

Module Export Configuration.

Record configuration : Type :=
    mkconfiguration
        { config_nchunks : Nchunks.t
        ; path_chunks : string
        ; path_db : string
        ; my_id : string
        ; trace : Tracer.tracer
        }.

End Configuration.