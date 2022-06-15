(**
      e L y K s e e R
*)

(** modeling backup of multiple files to LXR *)

Module Export BackupPlanner.

(** imports *)
From Coq Require Import Strings.String Strings.Byte Lists.List Lia.
(* Require Import Arith Number. *)
Require Import ZArith NArith PArith.
From Coq Require Import NArith.BinNat.
Open Scope positive_scope.
Open Scope N_scope.

From LXR Require Import Assembly.
From LXR Require Import Block.
From LXR Require Import Configuration.
From LXR Require Import Conversion.
From LXR Require Import Environment.
From LXR Require Import Filesupport.
From LXR Require Import Filetypes.

Open Scope string_scope.

(** aliases *)


(** 1 preparations *)


(** 2 setup environment *)

Definition assemblysz (c : configuration) : N := pos2N (chunkwidth * chunklength * (num_chunks c)).

Definition prepare_assembly (c : configuration) (e : environment) : environment :=
    let a := cur_assembly e in
    match (assemblysz c) - apos a with
    | 0 => (* TODO: extract cur_assembly and create a new assembly if apos >= alen *)
        (* extract_assembly a *)
        env_set_assembly e (new_assembly a)
    | _ => e
    end.

(** 3 backup *)

Definition backup_block (idx : positive) (fi : fileinformation) (wrote : N) (c : configuration) (e : environment) : (blockinformation * environment) :=
    let a0 := cur_assembly e in
    let apos0 := apos a0 in
    let avsz := assemblysz c - apos0 in
    let rsz := fsize fi - wrote in
    let bsz := N.min avsz rsz in
    let a1 := Assembly.add_data bsz a0 in     (** TODO write block to assembly *)
    let e2 := env_set_assembly e a1 in
    let e3 := prepare_assembly c e2 in    (* renew assembly in case full; TODO create aid from anum *)
    let bi := {| blockid := idx; blocksize := bsz;
                 filepos := wrote;
                 blockaid := anum a1; blockapos := apos0 |} in
    (bi, e3).

Program Fixpoint backup_blocks (idx : nat) (fi : fileinformation) (wrote : N) (bidx : positive) (bis : list blockinformation) (c : configuration) (e : environment) : environment :=
    match idx with
    | O => env_add_file e {| bfi := fi; blocks := bis |}
    | S p => let (bi, e2) := backup_block bidx fi wrote c e in
             backup_blocks p fi (wrote + blocksize bi) (bidx + 1) (bi :: bis) c e2
    end.

Definition calc_num_blocks (fi : fileinformation) (c : configuration) (e : environment) : N :=
    let asz := assemblysz c in
    let reqsz := fsize fi + apos (cur_assembly e) in
    let modsz := reqsz mod asz in
    match modsz with
    | 0 => reqsz / asz
    | _ => reqsz / asz + 1
    end.

(* start with fresh assembly *)
Eval compute in
    let filesize := 8388608 in
    let nchunks := 16%positive in
    let c := {| num_chunks := nchunks; path_chunks := "./lxr"; path_meta := "./meta" |} in
    calc_num_blocks {| fname := "t1.dat"; fsize := filesize; fowner := "me"; fpermissions := 644; fmodified := "some"; fchecksum := "ABDC" |}
                    c
                    (initial_environment c) = (* 2 *) filesize / pos2N(nchunks) / pos2N(Assembly.chunkwidth) / pos2N(Assembly.chunklength) .
(* already 4M in assembly *)
Eval compute in 
    let c := {| num_chunks := 16; path_chunks := "./lxr"; path_meta := "./meta" |} in
    let e0 := (initial_environment c) in
    let a0 := cur_assembly e0 in
    let a1 := add_data 4194304 a0 in
    let e := {| cur_assembly := a1
              ; count_input_bytes := 4194304
              ; files := {| bfi := {| fname := "t0.dat"; fsize := 4194304; fowner := "me"; fpermissions := 644; fmodified := "some"; fchecksum := "DEFA" |}
                          ; blocks := {| blockid := 1; blocksize := 4194304; filepos := 0; blockaid := 1; blockapos := 0 |} :: nil |} :: nil |} in
    calc_num_blocks {| fname := "t1.dat"; fsize := 8388608; fowner := "me"; fpermissions := 644; fmodified := "some"; fchecksum := "ABDC" |}
                    c
                    e = 2 .
(* already 4M+1 in assembly *)
Eval compute in 
    let c := {| num_chunks := 16; path_chunks := "./lxr"; path_meta := "./meta"; my_id := 321456 |} in
    let e0 := (initial_environment c) in
    let a1 := add_data 1 (new_assembly (cur_assembly e0)) in
    let e := {| cur_assembly := a1
              ; count_input_bytes := 4194305
              ; config := c
              ; files := {| bfi := {| fname := "t0.dat"; fsize := 4194304; fowner := "me"; fpermissions := 644; fmodified := "some"; fchecksum := "DEFA" |}
                          ; blocks := {| blockid := 1; blocksize := 4194304; filepos := 0; blockaid := anum a1; blockapos := apos a1 |} :: nil |} :: nil |} in
    calc_num_blocks {| fname := "t1.dat"; fsize := 8388608; fowner := "me"; fpermissions := 644; fmodified := "some"; fchecksum := "ABDC" |}
                    c
                    e = 3 .

Definition backup_file' (c : configuration) (e : environment) (fi : fileinformation) : environment :=
    let num_blocks := calc_num_blocks fi c e in
    backup_blocks (N.to_nat num_blocks) fi 0%N 1 nil c e.
Definition backup_file (c : configuration) (e : environment) (f : filename) : environment :=
    let fi : fileinformation := get_file_information f in
    backup_file' c e fi.

(* already 4M, +1M in assembly *)
Eval compute in 
    let c := {| num_chunks := 16; path_chunks := "./lxr"; path_meta := "./meta"; my_id := 789001 |} in
    let e0 := (initial_environment c) in
    let a1 := add_data 4194304 (new_assembly (cur_assembly e0)) in
    let e := {| cur_assembly := a1
              ; count_input_bytes := 4194304
              ; config := c
              ; files := {| bfi := {| fname := "t0.dat"; fsize := 4194304; fowner := "me"; fpermissions := 644; fmodified := "some"; fchecksum := "DEFA" |}
                          ; blocks := {| blockid := 1; blocksize := 4194304; filepos := 0; blockaid := anum a1; blockapos := apos a1 |} :: nil |} :: nil |} in
    backup_file' c e {| fname := "t1.dat"; fsize := 1048576; fowner := "me"; fpermissions := 644; fmodified := "some"; fchecksum := "ABDC" |} = e0.


(** 4 termination & cleanup *)


End BackupPlanner.
