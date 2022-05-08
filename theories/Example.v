(**
      e L y K s e e R
*)

(** example backup *)

Module Export Example.

(** imports *)
From Coq Require Import Strings.String Strings.Byte Lists.List Lia.
Require Import PArith NArith.
Open Scope positive_scope.
Open Scope N_scope.

From LXR Require Import Backup.
From LXR Require Import Filesupport.
From LXR Require Import Filetypes.

Open Scope string_scope.

(** defs *)

Example config0 : configuration :=
    {| num_chunks := 16
     ; path_chunks := "./lxr"
     ; path_meta := "./meta"
    |}.

Require Import LXR.Assembly.

Example env0 (c : configuration) : environment :=
    initial_environment c.

Lemma test1 : 
    let e := env0 config0 in
    let fi := {| fname := "t1.dat"; fsize := 33; fowner := "me"; fpermissions := 644; fmodified := ""; fchecksum := "" |} in
    let bi := {| blockid := 1; blocksize := 33; blockaid := 1 |} in
    backup_file config0 e "t1.dat" =
    {| cur_assembly := first_assembly (num_chunks config0) 
    ;  count_input_bytes := 33
    ;  files := fi :: nil
    ;  blocks := bi :: nil
    |}.
Proof. Admitted.


End Example.
