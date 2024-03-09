(**
      e L y K s e e R
*)

Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String Program.Basics.

From RecordUpdate Require Import RecordUpdate.

From LXR Require Import Assembly.
From LXR Require Import Configuration.
From LXR Require Import Filesupport.

Open Scope list_scope.

Import ListNotations.

Module Export Store.

Definition RecordStore := Type.
Record store (KVs : Type): RecordStore :=
    mkstore
        { sconfig : configuration
        ; entries : KVs
        }.
(* Print RecordStore. *)
(* Print store. *)

(* Print option. *)

Fixpoint rec_find {V : Type} (k : string) (es : list (string * V)) : option V :=
    match es with
    | [] => None
    | (k', v')::r => if k' =? k then Some v'
                     else rec_find k r
    end.

Module Type STORE.
    Parameter K : Type.
    Parameter V : Type.
    Parameter KVs : Type.
    Parameter R : Type.
    Parameter init : configuration -> R.
    Parameter add : K -> V -> R -> R.
    Parameter find : K -> R -> option V.
End STORE.
(* Print STORE. *)

Module KeyListStore <: STORE.
    Definition K := String.string.
    Definition V := Assembly.keyinformation.
    Definition KVs := list (K * V).
    Definition R : RecordStore := store KVs.
    Definition init (c : configuration) : R := {| sconfig := c; entries := [] |}.
    Definition add (k : K) (v : V) (r : R) : R :=
        {| sconfig := r.(sconfig KVs); entries := (k, v) :: r.(entries KVs) |}.
    Definition find (k : K) (r : R) : option V :=
        rec_find k r.(entries KVs).
End KeyListStore.
(* Print KeyListStore. *)

Module FBlockListStore <: STORE.
    Definition K := Assembly.aid_t.
    Definition V := Assembly.blockinformation.
    Definition KVs := list (K * V).
    Definition R : RecordStore := store KVs.
    Definition init (c : configuration) : R := {| sconfig := c; entries := [] |}.
    Definition add (k : K) (v : V) (r : R) : R :=
        {| sconfig := r.(sconfig KVs); entries := (k, v) :: r.(entries KVs) |}.
    Definition find (k : K) (r : R) : option V :=
        rec_find k r.(entries KVs).

End FBlockListStore.
(* Print FBlockListStore. *)

Module FileinformationStore <: STORE.
    Definition K := String.string.
    Definition V := Filesupport.fileinformation.
    Definition KVs := list (K * V).
    Definition R : RecordStore := store KVs.
    Definition init (c : configuration) : R := {| sconfig := c; entries := [] |}.
    Definition add (k : K) (v : V) (r : R) : R :=
        {| sconfig := r.(sconfig KVs); entries := (k, v) :: r.(entries KVs) |}.
    Definition find (k : K) (r : R) : option V :=
        rec_find k r.(entries KVs).

End FileinformationStore.
(* Print FileinformationStore. *)


Example find_entry_in_empty : forall c,
    let es := FBlockListStore.init c in
    let k : Assembly.aid_t := "t1" in
    FBlockListStore.find k es = None. 

Proof.
    intros.
    unfold FBlockListStore.find.
    unfold rec_find. simpl. reflexivity.
Qed.

Example add_then_find_entry : forall c,
    let es := FBlockListStore.init c in
    let v := {| blockid := 1; bchecksum := "chksum"; blocksize := 1024; filepos := 0; blockaid := "aid001"; blockapos := 42 |} in
    let k : Assembly.aid_t := "t1" in
    FBlockListStore.find k (FBlockListStore.add k v es) = Some v. 

Proof.
    intros.
    unfold FBlockListStore.add.
    unfold FBlockListStore.find.
    unfold rec_find. simpl. reflexivity.
Qed.

Example add_add_then_find_entry : forall c,
    let es := FBlockListStore.init c in
    let v1 := {| blockid := 1; bchecksum := "chksum"; blocksize := 1024; filepos := 0; blockaid := "aid001"; blockapos := 42 |} in
    let v2 := {| blockid := 2; bchecksum := "chksum"; blocksize := 1024; filepos := 42; blockaid := "aid001"; blockapos := 1066 |} in
    let k1 : Assembly.aid_t := "t1" in
    let k2 : Assembly.aid_t := "t2" in
    FBlockListStore.find k1 (FBlockListStore.add k2 v2 (FBlockListStore.add k1 v1 es)) = Some v1.

Proof.
    intros.
    unfold FBlockListStore.add. simpl.
    unfold FBlockListStore.find.
    unfold rec_find. simpl. reflexivity.
Qed.

Example add_then_find_another_entry : forall c,
    let es := FBlockListStore.init c in
    let v := {| blockid := 1; bchecksum := "chksum"; blocksize := 1024; filepos := 0; blockaid := "aid001"; blockapos := 42 |} in
    let k1 : Assembly.aid_t := "t1" in
    let k2 : Assembly.aid_t := "t2" in
    FBlockListStore.find k2 (FBlockListStore.add k1 v es) = None. 

Proof.
    intros.
    unfold FBlockListStore.add.
    unfold FBlockListStore.find.
    unfold rec_find. simpl. reflexivity.
Qed.

End Store.