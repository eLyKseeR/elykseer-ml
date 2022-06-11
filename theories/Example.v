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

From LXR Require Import Assembly.
From LXR Require Import BackupPlanner.
From LXR Require Import Block.
From LXR Require Import Configuration.
From LXR Require Import Environment.
From LXR Require Import Filesupport.
From LXR Require Import Filetypes.

Open Scope string_scope.

(** defs *)

Example config0 : configuration :=
    {| num_chunks := 16
     ; path_chunks := "./lxr"
     ; path_meta := "./meta"
    |}.


Example fi_t1_dat : fileinformation := {| fname := "t1.dat"; fsize := 4194304; fowner := "me"; fpermissions := 644; fmodified := ""; fchecksum := "" |}.

Lemma test1 : 
    let e := initial_environment config0 in
    let fi := fi_t1_dat in
    let bi := {| blockid := 1; blocksize := 4194304; filepos := 0; blockaid := 1; blockapos := 0 |} in
    backup_file config0 e "t1.dat" =
    {| cur_assembly := {|
                        nchunks := 16;
                        aid := 2;
                        valid := valid_assembly_size 16;
                        apos := 0; (*N.min (assemblysz config0) 33;*)
                        encrypted := false;
                        (* chunks := mk_chunk_list 16 1 *)
                       |} 
    ;  count_input_bytes := 4194304
    ;  files := {| bfi:= fi;
                   blocks := bi :: nil |} :: nil
    |}.
Proof. intros.
    unfold backup_file.
    replace (get_file_information "t1.dat") with fi.
    vm_compute (backup_file' config0 e fi).
    simpl. reflexivity.
 Admitted. (* the remaining: fi = get_file_information "t1.dat" *)


(* test the case where t1.dat (4M) fills the first asssembly,
   and then t2.dat (1M) is backuped. *)
Example fi_t2_dat : fileinformation := {| fname := "t2.dat"; fsize := 1048576; fowner := "me"; fpermissions := 644; fmodified := ""; fchecksum := "" |}.

Lemma test2 :
    let e0 := initial_environment config0 in
    let fi1 := fi_t1_dat in
    let bi1 := {| blockid := 1; blocksize := 4194304; filepos := 0; blockaid := 1; blockapos := 0 |} in
    let e1 := backup_file' config0 e0 fi1 in
    let fi2 := fi_t2_dat in
    backup_file config0 e1 "t2.dat" =
    {| cur_assembly := {|
                        nchunks := 16;
                        aid := 2;
                        valid := valid_assembly_size 16;
                        apos := 1048576;
                        encrypted := false;
                        (* chunks := mk_chunk_list 16 1 *)
                       |} 
    ;  count_input_bytes := 4194304 + 1048576
    ;  files := {| bfi:= fi_t2_dat
                  ; blocks := {| blockid := 1; blocksize := 1048576; filepos := 0; blockaid := 2; blockapos := 0 |} :: nil |}
                ::  {| bfi := fi1
                     ; blocks := bi1 :: nil |} :: nil
    |}.
Proof. intros.
    vm_compute (backup_file' config0 e0 fi1) in * |-.
    unfold backup_file.
    replace (get_file_information "t2.dat") with fi2.
    vm_compute (backup_file' config0 e1 fi2).

    * simpl.
      reflexivity.
    * Admitted. (* the remaining: fi2 = get_file_information "t2.dat" *)

End Example.
