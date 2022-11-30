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
Open Scope string_scope.

From LXR Require Import Nchunks Buffer Configuration Conversion Utilities.


Definition chunkwidth  : positive := 256%positive.
Definition chunklength : positive := 1024%positive.
Definition chunksize   : positive := chunkwidth * chunklength.
Definition chunksize_N : N := Conversion.pos2N chunksize.
Definition assemblysize (n : Nchunks.Private.t) : N := chunksize_N * (to_N n).

Definition aid_t := string.

Record assemblyinformation : Type :=
    mkassembly
        { nchunks : Nchunks.Private.t
        ; aid : aid_t
        ; apos : N }.

Record keyinformation : Type :=
    mkkeyinformation
        { ivec : string
        ; pkey : string
        ; localid : N
        ; localnchunks : positive
        }.


Module Type ASS.
    Definition H : Type := assemblyinformation.
    Parameter B : Type.
    (* Parameter assembly : Type. *)
    Axiom create : configuration -> (H * B).
    Axiom buffer_len : B -> N.
    Axiom calc_checksum : B -> string.
End ASS.

Module AssemblyPlainWritable : ASS.
    Definition H : Type := assemblyinformation.
    Definition B := BufferPlain.buffer_t.
    Definition buffer_len : B -> N := BufferPlain.buffer_len.
    Definition calc_checksum : B -> string := fun _ => "<>".
    Definition create (c : configuration) : H * B :=
        let chunks := config_nchunks c in
        let b := BufferPlain.buffer_create (chunksize_N * Nchunks.to_N chunks) in
        (mkassembly chunks (Utilities.rnd256 (my_id c)) 0, b).
End AssemblyPlainWritable.

Module AssemblyEncrypted : ASS.
    Definition H : Type := assemblyinformation.
    Definition B := BufferEncrypted.buffer_t.
    Definition buffer_len : B -> N := BufferEncrypted.buffer_len.
    Definition calc_checksum : B -> string := BufferEncrypted.calc_checksum.
    Definition create (c : configuration) : H * B :=
        let chunks := config_nchunks c in
        let b := BufferEncrypted.buffer_create (chunksize_N * Nchunks.to_N chunks) in
        (mkassembly chunks (Utilities.rnd256 (my_id c)) 0, b).
End AssemblyEncrypted.
(* Print AssemblyEncrypted. *)

Module AssemblyPlainFull : ASS.
    Definition H : Type := assemblyinformation.
    Definition B := BufferPlain.buffer_t.
    Definition buffer_len : B -> N := BufferPlain.buffer_len.
    Definition calc_checksum : B -> string := BufferPlain.calc_checksum.
    Definition create (c : configuration) :=
        let chunks := config_nchunks c in
        let sz := chunksize_N * Nchunks.to_N chunks in
        let b := BufferPlain.buffer_create sz in
        (mkassembly chunks (Utilities.rnd256 (my_id c)) sz, b).
End AssemblyPlainFull.


Section Code_Encrypted.
(** operations on AssemblyEncrypted *)

Axiom id_buffer_t_from_enc : AssemblyEncrypted.B -> BufferEncrypted.buffer_t.
Axiom id_enc_from_buffer_t : BufferEncrypted.buffer_t -> AssemblyEncrypted.B.
Axiom id_assembly_plain_buffer_t_from_buf : BufferPlain.buffer_t -> AssemblyPlainWritable.B.
Program Definition decrypt (a : AssemblyEncrypted.H) (b : AssemblyEncrypted.B) (ki : keyinformation) : option (AssemblyPlainWritable.H * AssemblyPlainWritable.B) :=
    let a' := mkassembly (nchunks a) (aid a) 0 in
    let bdec := Buffer.decrypt (id_buffer_t_from_enc b) (ivec ki) (pkey ki) in
    let b' := id_assembly_plain_buffer_t_from_buf bdec in
    Some (a', b').

Axiom chunk_identifier : configuration -> aid_t -> positive -> string.
Axiom chunk_identifier_path : configuration -> aid_t -> positive -> string.

Axiom ext_load_chunk_from_path : string -> option BufferEncrypted.buffer_t.

Program Definition recall (c : configuration) (a : AssemblyEncrypted.H) : option (AssemblyEncrypted.H * AssemblyEncrypted.B) :=
    let cidlist := Utilities.make_list (nchunks a) in
    let b := BufferEncrypted.buffer_create (Conversion.pos2N (nchunks a) * chunksize_N) in
    let aid := aid a in
    let blen := BufferEncrypted.buffer_len b in
    let nread := List.fold_left
                    (fun nread cid =>
                        let cpath := chunk_identifier_path c aid cid in
                        match ext_load_chunk_from_path cpath with
                        | None => nread
                        | Some cb =>
                            let apos := chunksize_N * ((Conversion.pos2N cid) - 1) in
                            if N.leb (apos + chunksize_N) blen
                            then nread + BufferEncrypted.copy_sz_pos cb 0 chunksize_N b apos
                            else nread
                        end    
                    )
                    cidlist
                    0 in
    let a' := mkassembly (nchunks a) aid nread in
    let b' := id_enc_from_buffer_t b in
    Some (a', b').

Axiom ext_store_chunk_to_path : string -> N -> N -> BufferEncrypted.buffer_t -> N.
Program Definition extract (c : configuration) (a : AssemblyEncrypted.H) (b : AssemblyEncrypted.B) : N :=
    let aid := aid a in
    let buf := id_buffer_t_from_enc b in
    List.fold_left
        ( fun nwritten cid =>
            let cpath := chunk_identifier_path c aid cid in
            let apos := chunksize_N * ((Conversion.pos2N cid) - 1) in
            nwritten + ext_store_chunk_to_path cpath chunksize_N apos buf
        )
        (Utilities.make_list (nchunks a))
        0.

End Code_Encrypted.

Section Code_Plain.
(** operations on AssemblyPlain *)

Axiom id_buffer_t_from_full : AssemblyPlainFull.B -> BufferPlain.buffer_t.
Axiom id_buffer_t_from_writable : AssemblyPlainWritable.B -> BufferPlain.buffer_t.
Axiom id_assembly_enc_buffer_t_from_buf : BufferEncrypted.buffer_t -> AssemblyEncrypted.B.
Axiom id_assembly_full_buffer_from_writable : AssemblyPlainWritable.B -> AssemblyPlainFull.B.

Program Definition finish (a : AssemblyPlainWritable.H) (b : AssemblyPlainWritable.B) : (AssemblyPlainFull.H * AssemblyPlainFull.B) :=
    ( mkassembly (nchunks a) (aid a) (apos a)
    , id_assembly_full_buffer_from_writable b ).

Program Definition encrypt (a : AssemblyPlainFull.H) (b : AssemblyPlainFull.B) (ki : keyinformation) : option (AssemblyEncrypted.H * AssemblyEncrypted.B) :=
    let a' := mkassembly (nchunks a) (aid a) (assemblysize (nchunks a)) in
    let benc  := Buffer.encrypt (id_buffer_t_from_full b) (ivec ki) (pkey ki) in
    let b' := id_assembly_enc_buffer_t_from_buf benc in
    Some (a', b').

(** backup: add a buffer to an assembly
    return blockinformation *)

Record blockinformation : Type := mkblockinformation
    { blockid : positive
    ; bchecksum : string
    ; blocksize : N
    ; filepos : N
    ; blockaid : string
    ; blockapos : N
    }.

Axiom assembly_add_content : BufferPlain.buffer_t -> N -> N -> AssemblyPlainWritable.B -> N.
Program Definition backup (a : AssemblyPlainWritable.H) (b : AssemblyPlainWritable.B) (fpos : N) (content : BufferPlain.buffer_t) : (AssemblyPlainWritable.H * blockinformation) :=
    let apos_n := apos a in
    let bsz := BufferPlain.buffer_len content in
    let chksum := calc_checksum content in
    let nwritten := assembly_add_content content bsz apos_n b in
    let bi := {| blockid   := 1
               ; bchecksum := chksum
               ; blocksize := nwritten
               ; filepos   := fpos
               ; blockaid  := aid a
               ; blockapos := apos_n |} in
    let a' := {| nchunks := nchunks a; aid := aid a; apos := apos_n + nwritten |} in
    (a', bi).

(** restore: copy buffer from an assembly
    and return it *)
Axiom assembly_get_content : AssemblyPlainFull.B -> N -> N -> BufferPlain.buffer_t -> N.
Program Definition restore (b : AssemblyPlainFull.B) (bi : blockinformation) : option BufferPlain.buffer_t :=
    let bsz := blocksize bi in
    let b' := BufferPlain.buffer_create bsz in
    let nw := assembly_get_content b bsz (blockapos bi) b' in
    if N.eqb nw bsz then
        let bcksum := calc_checksum b' in
        if String.eqb bcksum (bchecksum bi) then
            Some b'
        else
            None
    else None.

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
