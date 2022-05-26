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

Require Import LXR.Assembly.

Example env0 (c : configuration) : environment :=
    initial_environment c.

Example fi_t1_dat : fileinformation := {| fname := "t1.dat"; fsize := 33; fowner := "me"; fpermissions := 644; fmodified := ""; fchecksum := "" |}.

Lemma test1 : 
    let e := env0 config0 in
    let fi := fi_t1_dat in
    let bi := {| blockid := 1; blocksize := 33; filepos := 0; blockaid := 1; blockapos := 0 |} in
    backup_file config0 e "t1.dat" =
    {| cur_assembly := {|
                        nchunks := 16;
                        aid := 1;
                        valid := valid_assembly_size 16;
                        apos := 33; (*N.min (assemblysz config0) 33;*)
                        encrypted := false;
                        chunks := mk_chunk_list 16 1
                       |} 
    ;  count_input_bytes := 33
    ;  files := {| bfi:= fi; blocks := bi :: nil |} :: nil
    |}.
Proof. intros.
    unfold backup_file.
    replace (get_file_information "t1.dat") with fi.
    simpl.
    vm_compute (N.min (assemblysz config0) 33).
    
    unfold add_data.
    unfold env_set_assembly.
    unfold env_add_file. simpl.

    reflexivity.
 Admitted. (* the remaining: fi = get_file_information "t1.dat" *)


End Example.
