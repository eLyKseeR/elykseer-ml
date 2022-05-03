(**
      e L y K s e e R
*)

(** modeling backup of multiple files to LXR *)

Module Export Backup.

(** imports *)
From Coq Require Import Strings.String Strings.Byte Lists.List Lia.
(* Require Import Arith Number. *)
Require Import ZArith NArith PArith.
From Coq Require Import NArith.BinNat.
Open Scope positive_scope.
Open Scope N_scope.

From LXR Require Import Assembly.
From LXR Require Import Conversion.
From LXR Require Import Filesupport.
From LXR Require Import Filetypes.

Open Scope string_scope.

(** aliases *)

Definition block := list byte.
Record blockinformation : Type :=
    mkblockinformation
        { blockid : N
        ; blocksize : N
        ; blockaid : positive
        }.

(** 1 preparations *)

Record configuration : Type :=
    mkconfiguration
        { num_chunks : positive
        ; path_chunks : string
        ; path_meta : string
        }.

(** 2 setup environment *)

Record environment : Type :=
    mkenvironment
        { cur_assembly : assembly
        ; count_input_bytes : N
        ; files : list fileinformation
        ; blocks : list blockinformation
        }.
Definition initial_environment (c : configuration) : environment :=
    {| cur_assembly := first_assembly (num_chunks c)
    ;  count_input_bytes := 0%N
    ;  files := nil
    ;  blocks := nil
    |}.
Definition env_set_assembly (e : environment) (a : assembly) : environment :=
    {| cur_assembly := a
    ;  count_input_bytes := count_input_bytes e
    ;  files := files e
    ;  blocks := blocks e
    |}.
Definition env_add_file (e : environment) (fi : fileinformation) : environment :=
    {| cur_assembly := cur_assembly e
    ; count_input_bytes := count_input_bytes e
    ; files := fi :: files e
    ; blocks := blocks e
    |}.
Definition env_add_block (e : environment) (b : blockinformation) : environment :=
    {| cur_assembly := cur_assembly e
    ; count_input_bytes := blocksize b + count_input_bytes e
    ; files := files e
    ; blocks := b :: blocks e
    |}.

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

(* Definition open_file (f : file) : fptr := 11.
Definition read_file (size : nat) (ptr : fptr) : block := x03 :: x14 :: xab :: x42 :: nil. *)
(* Fixpoint run_file (size : nat) (ptr : fptr) (e : environment) (act : environment -> block -> environment)
            { struct size }
            : environment :=
    match size with
    | O => e
    | _ => let sz' := if Nat.leb 4 size then 4 else size in
            let b := read_file sz' ptr in
            let env' := act e b in
            run_file (size - sz') ptr env' act  end. *)

(* Definition with_file (fi : fileinformation) (e : environment) (act : environment -> block -> environment) : environment :=
    let ptr : fptr := open_file (fname fi) in
    let env : environment := run_file (fsize fi) fptr e act in
    env. *)

Definition backup_block (idx : nat) (fi : fileinformation) (wrote : N) (c : configuration) (e : environment) : (N * environment) :=
    let e1 := prepare_assembly c e in
    let avsz := (assemblysz c) - apos (cur_assembly e1) in
    let rsz := (fsize fi) - wrote in
    let bsz := N.min avsz rsz in
    (** write block to assembly *)
    let e2 := env_add_block e1 {| blockid := N.of_nat idx; blocksize := bsz; blockaid := aid (cur_assembly e1) |} in
    (bsz, e2).

Program Fixpoint backup_blocks (idx : nat) (fi : fileinformation) (wrote : N) (c : configuration) (e : environment) : environment :=
    match idx with
    | O => env_add_file e fi
    | S p => let (w, e2) := backup_block idx fi wrote c e in
             backup_blocks p fi (wrote + w) c e2
    end.

Definition backup_file (c : configuration) (e : environment) (f : filename) : environment :=
    let fi : fileinformation := get_file_information f in
    let asz := assemblysz c in
    let fstbsz := asz - apos (cur_assembly e) in
    let num_blocks := (((fsize fi) - fstbsz) / asz) + 1 in
    backup_blocks (N.to_nat num_blocks) fi 0%N c e.

(** 4 termination & cleanup *)


End Backup.
