(**
      e L y K s e e R
*)

Module Export Assembly.

(**
 Module: Assembly
 Description: an assemblyinformation is an ordering of chunks of data,
              either plain for reading and writing, 
              or encrypted for longterm storage.
 *)

Set Implicit Arguments.
Unset Strict Implicit.
(* Unset Printing Implicit Defensive. *)

From Coq Require Import Strings.String .
Require Import ZArith NArith PArith.
From Coq Require Import NArith.BinNat.
Open Scope positive_scope.
Open Scope N_scope.
Open Scope list_scope.

From LXR Require Import Nchunks Buffer Configuration Conversion Utilities.
From LXR Require Import RelationFileAid RelationAidKey.


Module Export Nchunks.
    Module Private.
        Definition t := positive.
        Definition minimum : t := 16%positive.
        Definition maximum : t := 256%positive.
        Definition from_positive : positive -> t := fun n =>
            Pos.min maximum (Pos.max n minimum).
        Definition from_int : int -> t := fun i =>
            from_positive (Conversion.i2p i).
        Definition to_positive : t -> positive := fun x => x.
        Definition to_N : t -> N := fun x => Conversion.pos2N x.
    End Private.

Definition from_positive : positive -> Private.t := Private.from_positive.
Definition from_int : int -> Private.t := Private.from_int.
Definition to_positive : Private.t -> positive := Private.to_positive.
Definition to_N : Private.t -> N := Private.to_N.

End Nchunks.

Definition chunkwidth  : positive := 256%positive.
Definition chunklength : positive := 1024%positive.
Definition chunksize   : positive := chunkwidth * chunklength.
Definition chunksize_N : N := Conversion.pos2N chunksize.
Definition assemblysize (n : Nchunks.Private.t) : N := chunksize_N * (to_N n).

Definition aid_t := string.

Record assemblyinformation : Type := mkassembly
    { nchunks : Nchunks.Private.t
    ; aid : aid_t
    ; apos : N }.


Module Type ASS.
    Definition H : Type := assemblyinformation.
    Parameter B : Type.
    (* Parameter assembly : Type. *)
    Axiom create : configuration -> Nchunks.Private.t -> (H * B).
    Axiom buffer_len : B -> N.
    Axiom calc_checksum : B -> string.
End ASS.

Module AssemblyPlainWritable : ASS.
    Definition H : Type := assemblyinformation.
    Definition B := BufferPlain.buffer_t.
    Definition buffer_len : B -> N := BufferPlain.buffer_len.
    Definition calc_checksum : B -> string := fun _ => "<>".
    Definition create (c : configuration) (n : Nchunks.Private.t) : H * B :=
        let b := BufferPlain.buffer_create (chunksize_N * Conversion.pos2N n) in
        (mkassembly n (Utilities.rnd256 (my_id c)) 0, b).
End AssemblyPlainWritable.

Module AssemblyEncrypted : ASS.
    Definition H : Type := assemblyinformation.
    Definition B := BufferEncrypted.buffer_t.
    Definition buffer_len : B -> N := BufferEncrypted.buffer_len.
    Definition calc_checksum : B -> string := BufferEncrypted.calc_checksum.
    Definition create (c : configuration) (n : Nchunks.Private.t) :=
        let b := BufferEncrypted.buffer_create (chunksize_N * Conversion.pos2N n) in
        (mkassembly n (Utilities.rnd256 (my_id c)) 0, b).
End AssemblyEncrypted.
(* Print AssemblyEncrypted. *)

Module AssemblyPlainFull : ASS.
    Definition H : Type := assemblyinformation.
    Definition B := BufferPlain.buffer_t.
    Definition buffer_len : B -> N := BufferPlain.buffer_len.
    Definition calc_checksum : B -> string := BufferPlain.calc_checksum.
    Definition create (c : configuration) (n : Nchunks.Private.t) :=
        let sz := chunksize_N * Conversion.pos2N n in
        let b := BufferPlain.buffer_create sz in
        (mkassembly n (Utilities.rnd256 (my_id c)) sz, b).
End AssemblyPlainFull.


Section Code_Encrypted.
(** operations on AssemblyEncrypted *)

Axiom id_buffer_t_from_enc : AssemblyEncrypted.B -> BufferEncrypted.buffer_t.
Axiom id_enc_from_buffer_t : BufferEncrypted.buffer_t -> AssemblyEncrypted.B.
Axiom id_assembly_plain_buffer_t_from_buf : BufferPlain.buffer_t -> AssemblyPlainWritable.B.
Program Definition decrypt (a : AssemblyEncrypted.H) (b : AssemblyEncrypted.B) (rel : RelationAidKey.Map) : option (AssemblyPlainWritable.H * AssemblyPlainWritable.B) :=
    match RelationAidKey.find (aid a) rel with
    | None => None
    | Some pw =>
        let a' := mkassembly (nchunks a) (aid a) 0 in
        let bdec := Buffer.decrypt (id_buffer_t_from_enc b) (pkey pw) in
        let b' := id_assembly_plain_buffer_t_from_buf bdec in
        Some (a', b')
    end.

Axiom chunk_identifier : configuration -> aid_t -> positive -> string.
Axiom chunk_identifier_path : configuration -> aid_t -> positive -> string.

Axiom cpp_load_chunk_from_path : string -> option BufferEncrypted.buffer_t.
Fixpoint recall_chunks (nread : N) (c : configuration) (aid : aid_t) (b : BufferEncrypted.buffer_t) (cids : list positive) : N :=
    match cids with
    | nil => nread
    | cid :: rcids =>
        let cpath := chunk_identifier_path c aid cid in
        match cpp_load_chunk_from_path cpath with
        | None => nread
        | Some cb =>
            let apos := chunksize_N * ((Conversion.pos2N cid) - 1) in
            if N.ltb (apos + chunksize_N) (BufferEncrypted.buffer_len b)
            then let nread' := BufferEncrypted.copy_sz_pos cb 0 chunksize_N b apos in
                recall_chunks (nread + nread') c aid b rcids
            else nread
        end
    end.

Program Definition recall (c : configuration) (a : AssemblyEncrypted.H) : option (AssemblyEncrypted.H * AssemblyEncrypted.B) :=
    let cidlist := Utilities.make_list (nchunks a) in
    let b := BufferEncrypted.buffer_create (Conversion.pos2N (nchunks a) * chunksize_N) in
    let nread := recall_chunks 0 c (aid a) b cidlist in
    let a' := mkassembly (nchunks a) (aid a) nread in
    let b' := id_enc_from_buffer_t b in
    Some (a', b').

Axiom cpp_store_chunk_to_path : string -> N -> N -> BufferEncrypted.buffer_t -> N.
Fixpoint extract_chunks (written : N) (c : configuration) (aid : aid_t) (b : BufferEncrypted.buffer_t) (cids : list positive) : N :=
    match cids with
    | nil => written
    | cid :: rcids =>
        let cpath := chunk_identifier_path c aid cid in
        let apos := chunksize_N * ((Conversion.pos2N cid) - 1) in
        let nwritten := cpp_store_chunk_to_path cpath chunksize_N apos b in
        extract_chunks (written + nwritten) c aid b rcids
    end.

Program Definition extract (c : configuration) (a : AssemblyEncrypted.H) (b : AssemblyEncrypted.B) : N :=
    let cidlist := Utilities.make_list (nchunks a) in
    extract_chunks 0 c (aid a) (id_buffer_t_from_enc b) cidlist.

End Code_Encrypted.

Section Code_Plain.
(** operations on AssemblyPlain *)

Axiom id_buffer_t_from_full : AssemblyPlainFull.B -> BufferPlain.buffer_t.
Axiom id_buffer_t_from_writable : AssemblyPlainWritable.B -> BufferPlain.buffer_t.
Axiom id_assembly_enc_buffer_t_from_buf : BufferEncrypted.buffer_t -> AssemblyEncrypted.B.
Axiom id_assembly_full_buffer_from_writable : AssemblyPlainWritable.B -> AssemblyPlainFull.B.

Program Definition finish (a : AssemblyPlainWritable.H) (b : AssemblyPlainWritable.B) : (AssemblyPlainFull.H * AssemblyPlainFull.B) :=
    ( mkassembly (nchunks a) (aid a) (apos a)
    , id_assembly_full_buffer_from_writable b).

Program Definition encrypt (a : AssemblyPlainFull.H) (b : AssemblyPlainFull.B) (rel : RelationAidKey.Map) : option (AssemblyEncrypted.H * AssemblyEncrypted.B) :=
    match RelationAidKey.find (aid a) rel with
    | None => None
    | Some pw => let a' := mkassembly (nchunks a) (aid a) (assemblysize (nchunks a)) in
                 let benc  := Buffer.encrypt (id_buffer_t_from_full b) (pkey pw) in
                 let b' := id_assembly_enc_buffer_t_from_buf benc in
                 Some (a', b')
    end.

(** backup: add a buffer to an assembly
    return updated map *)
Program Definition backup (a : AssemblyPlainWritable.H) (b : AssemblyPlainWritable.B) (fp : string) (content : BufferPlain.buffer_t) (rel : RelationFileAid.Map) : (AssemblyPlainWritable.H * RelationFileAid.Map) :=
    let apos := apos a in
    let bsz := BufferPlain.buffer_len content in
    let rentries := match RelationFileAid.find fp rel with
    | None =>  {| blockid   := 1
                ; bchecksum := BufferPlain.calc_checksum content
                ; blocksize := bsz
                ; filepos   := 0  (* beginning *)
                ; blockaid  := aid a
                ; blockapos := 0 |} :: nil
    | Some nil =>
               {| blockid   := 1
                ; bchecksum := calc_checksum content
                ; blocksize := bsz
                ; filepos   := 0  (* beginning *)
                ; blockaid  := aid a
                ; blockapos := 0 |} :: nil
    | Some (re :: res) =>
               {| blockid   := 1 + (blockid re)
                ; bchecksum := calc_checksum content
                ; blocksize := bsz
                ; filepos   := (blocksize re) + (filepos re) (* assuming the blocks are sequential *)
                ; blockaid  := aid a
                ; blockapos := apos |} :: re :: res
    end in
    let nwritten := BufferPlain.copy_sz_pos content 0 bsz (id_buffer_t_from_writable b) apos in
    let a' := {| nchunks := nchunks a; aid := aid a; apos := apos + bsz |} in
    let rel' := RelationFileAid.add fp rentries rel in
    (a', rel').

(** restore: copy buffer from an assembly
    and return it *)
Program Definition restore (b : AssemblyPlainFull.B) (name : string) (bid : positive) (rel : RelationFileAid.Map) : option BufferPlain.buffer_t :=
    match RelationFileAid.find name rel with
    | None => None
    | Some bis => match List.filter (fun bi => Pos.eqb (blockid bi) bid) bis with
                  | nil => None
                  | bi :: _ => let bsz := blocksize bi in
                               let b' := BufferPlain.buffer_create bsz in
                               let _nw := BufferPlain.copy_sz_pos (id_buffer_t_from_full b) (blockapos bi) bsz b' 0 in
                               Some b'
                  end
    end.

End Code_Plain.

(* Section lemmas.

Variable c : configuration.
Variable a : assemblyinformation.

Fact external_decrypt_of_encrypt_id : forall (b : AssemblyPlainFull.B) (p : string), cpp_decrypt_assembly (cpp_encrypt_assembly b p) p = b.
Proof.
    intros.
    Admitted.

Lemma decrypt_of_encrypt_id :
    forall (b : AssemblyPlainFull.B) (b' : AssemblyEncrypted.B) (b'' : AssemblyPlainFull.B)  (map : RelationAidKey.Map), 
    encrypt a b map = Some b' /\ decrypt a b' map = Some b'' -> b'' = b.
Proof.
    intros.
    (* unfold decrypt. unfold encrypt. *)
    (* apply external_decrypt_of_encrypt_id. *)
    Admitted.

End lemmas. *)

End Assembly.
