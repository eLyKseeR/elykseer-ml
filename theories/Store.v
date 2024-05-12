(**
      e L y K s e e R
*)

Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String Program.Basics.

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

Fixpoint rec_find {V : Type} (k : string) (es : list (string * V)) : option V :=
    match es with
    | [] => None
    | (k', v')::r => if k' =? k then Some v'
                     else rec_find k r
    end.
Fixpoint rec_find_all {V : Type} (k : string) (es : list (string * V)) (agg : list V) : list V :=
    match es with
    | [] => agg
    | (k', v')::r => if k' =? k
                     then rec_find_all k r (v' :: agg)
                     else rec_find_all k r agg
    end.

Module Type STORE.
    Parameter K : Type.
    Parameter V : Type.
    Parameter KVs : Type.
    Parameter R : Type.
    Parameter init : configuration -> R.
    Parameter add : K -> V -> R -> R.
    Parameter find : K -> R -> option V.
    Parameter find_all : K -> R -> list V.
End STORE.
(* Print STORE. *)

Module KeyListStore <: STORE.
    Definition K := Assembly.aid_t.
    Definition V := Assembly.keyinformation.
    Definition KVs := list (K * V).
    Definition R : RecordStore := store KVs.
    Definition init (c : configuration) : R := {| sconfig := c; entries := [] |}.
    Definition add (k : K) (v : V) (r : R) : R :=
        {| sconfig := r.(sconfig KVs); entries := (k, v) :: r.(entries KVs) |}.
    Definition find (k : K) (r : R) : option V :=
        rec_find k r.(entries KVs).
    Definition find_all (k : K) (r : R) : list V := nil.
End KeyListStore.
(* Print KeyListStore. *)

Module FBlockListStore <: STORE.
    Definition K := String.string.
    Definition V := Assembly.blockinformation.
    Definition KVs := list (K * V).
    Definition R : RecordStore := store KVs.
    Definition init (c : configuration) : R := {| sconfig := c; entries := [] |}.
    Definition add (k : K) (v : V) (r : R) : R :=
        {| sconfig := r.(sconfig KVs); entries := (k, v) :: r.(entries KVs) |}.
    Definition find (k : K) (r : R) : option V := None.
    Definition find_all (k : K) (r : R) : list V :=
        rec_find_all k r.(entries KVs) [].

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
    Definition find_all (k : K) (r : R) : list V := nil.

End FileinformationStore.
(* Print FileinformationStore. *)


Example find_entry_in_empty : forall c,
    let es := FBlockListStore.init c in
    let k : string := "fhash" in
    FBlockListStore.find_all k es = nil. 

Proof.
    intros.
    unfold FBlockListStore.find_all.
    unfold rec_find. simpl. reflexivity.
Qed.

Example add_then_find_entry : forall c,
    let es := FBlockListStore.init c in
    let v := {| blockid := 1; bchecksum := "chksum"; blocksize := 1024; filepos := 0; blockaid := "aid001"; blockapos := 42 |} in
    let k : string := "fhash" in
    FBlockListStore.find_all k (FBlockListStore.add k v es) = [ v ]. 

Proof.
    intros.
    unfold FBlockListStore.add.
    unfold FBlockListStore.find_all.
    unfold rec_find. simpl. reflexivity.
Qed.

Example add_add_then_find_entry : forall c,
    let es := FBlockListStore.init c in
    let v1 := {| blockid := 1; bchecksum := "chksum"; blocksize := 1024; filepos := 0; blockaid := "aid001"; blockapos := 42 |} in
    let v2 := {| blockid := 2; bchecksum := "chksum"; blocksize := 1024; filepos := 42; blockaid := "aid001"; blockapos := 1066 |} in
    let k1 : string := "fhash1" in
    (* let k2 : string := "fhash2" in *)
    FBlockListStore.find_all k1 (FBlockListStore.add k1 v2 (FBlockListStore.add k1 v1 es)) = [ v1; v2 ].

Proof.
    intros.
    unfold FBlockListStore.add. simpl.
    unfold FBlockListStore.find_all.
    unfold rec_find. simpl. reflexivity.
Qed.

Example add_then_find_another_entry : forall c,
    let es := FBlockListStore.init c in
    let v := {| blockid := 1; bchecksum := "chksum"; blocksize := 1024; filepos := 0; blockaid := "aid001"; blockapos := 42 |} in
    let k1 : string := "fhash1" in
    let k2 : string := "fhash2" in
    FBlockListStore.find_all k2 (FBlockListStore.add k1 v es) = nil. 

Proof.
    intros.
    unfold FBlockListStore.add.
    unfold FBlockListStore.find_all.
    unfold rec_find. simpl. reflexivity.
Qed.

Example add_then_find_fileinformation : forall c,
    let es := FileinformationStore.init c in
    let v := {| fname := "file001"; fhash := "h(file001)"; fowner := "user"; fsize := 1024; fpermissions := 660; fmodified := "20210831T174218"; fchecksum := "42" |} in
    let k : string := "file001" in
    FileinformationStore.find k (FileinformationStore.add k v es) = Some v. 

Proof.
    intros.
    unfold FileinformationStore.add.
    unfold FileinformationStore.find.
    unfold rec_find.
    simpl. reflexivity.
Qed.


End Store.