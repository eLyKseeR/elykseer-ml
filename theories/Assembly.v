(**
      e L y K s e e R
*)

From Coq Require Import Strings.String Program.Basics.
Require Import ZArith NArith PArith.
From Coq Require Import NArith.BinNat.

From LXR Require Import Nchunks Cstdio Configuration Conversion Filesystem Utilities.

Module Export Assembly.

(**
 Module: Assembly
 Description: an assembly is an ordering of chunks of data,
              either plain for reading and writing, 
              or encrypted for longterm storage.
 *)

Set Implicit Arguments.
Unset Strict Implicit.

Open Scope positive_scope.
Open Scope N_scope.
Open Scope list_scope.
Open Scope string_scope.

Definition chunkwidth  : positive := 256%positive.
Definition chunklength : positive := 1024%positive.
Definition chunksize   : positive := chunkwidth * chunklength.
Definition chunksize_N : N := Conversion.pos2N chunksize.
Definition assemblysize (n : Nchunks.Private.t) : N := chunksize_N * (to_N n).

Definition aid_t := string.
Definition mkaid (c : configuration) : aid_t :=
    Utilities.rnd256 (my_id c).

Definition snapshot_t := positive.

Definition RecordAssemblyInformation := Set.
Record assemblyinformation : RecordAssemblyInformation :=
    mkassembly
        { nchunks : Nchunks.Private.t
        ; aid : aid_t
        ; apos : N }.

Definition RecordKeyInformation := Set.
Record keyinformation : RecordKeyInformation :=
    mkkeyinformation
        { ivec : string
        ; pkey : string
        ; localnchunks : positive
        }.

Definition RecordBlockInformation := Set.
Record blockinformation : RecordBlockInformation :=
    mkblockinformation
        { blockid : positive
        ; bchecksum : string
        ; blocksize : N
        ; filepos : N
        ; blockaid : aid_t
        ; blockapos : N
        }.

Module Type ASS.
    (* Parameter H : RecordAssemblyInformation. *)
    Parameter B : Type.
    Axiom create : configuration -> (assemblyinformation * B).
    Axiom buffer_len : B -> N.
    Axiom calc_checksum : B -> string.
End ASS.
(* Print ASS. *)

Module AssemblyPlainWritable : ASS.
    (* Definition H : RecordAssemblyInformation := assemblyinformation. *)
    Definition B := BufferPlain.buffer_t.
    Definition buffer_len : B -> N := BufferPlain.buffer_len.
    Definition calc_checksum : B -> string := fun _ => "<>".
    Definition create (c : configuration) : assemblyinformation * B :=
        let chunks := config_nchunks c in
        let b := BufferPlain.buffer_create (chunksize_N * Nchunks.to_N chunks) in
        let rb := Cstdio.ranbuf128 tt in
        let nb := BufferPlain.copy_sz_pos rb 0 16 b 0 in
        (mkassembly chunks (mkaid c) nb, b).
End AssemblyPlainWritable.
(* Print AssemblyPlainWritable. *)

Module AssemblyEncrypted : ASS.
    (* Definition H : RecordAssemblyInformation := assemblyinformation. *)
    Definition B := BufferEncrypted.buffer_t.
    Definition buffer_len : B -> N := BufferEncrypted.buffer_len.
    Definition calc_checksum : B -> string := BufferEncrypted.calc_checksum.
    Definition create (c : configuration) : assemblyinformation * B :=
        let chunks := config_nchunks c in
        let b := BufferEncrypted.buffer_create (chunksize_N * Nchunks.to_N chunks) in
        (mkassembly chunks (mkaid c) 0, b).
End AssemblyEncrypted.
(* Print AssemblyEncrypted. *)

Module AssemblyPlainFull : ASS.
    (* Definition H : RecordAssemblyInformation := assemblyinformation. *)
    Definition B := BufferPlain.buffer_t.
    Definition buffer_len : B -> N := BufferPlain.buffer_len.
    Definition calc_checksum : B -> string := BufferPlain.calc_checksum.
    Definition create (c : configuration) : assemblyinformation * B :=
        let chunks := config_nchunks c in
        let sz := chunksize_N * Nchunks.to_N chunks in
        let b := BufferPlain.buffer_create sz in
        (mkassembly chunks (mkaid c) sz, b).
End AssemblyPlainFull.
(* Print AssemblyPlainFull. *)

Local Program Definition set_apos (a : assemblyinformation) (apos : N) : assemblyinformation :=
    {| nchunks := a.(nchunks); aid := a.(aid); apos := apos; |}.

Section Code_Writeable.
(** operations on AssemblyPlainWriteable *)

Axiom id_assembly_full_ainfo_from_writable : assemblyinformation -> assemblyinformation.
Axiom id_assembly_full_buffer_from_writable : AssemblyPlainWritable.B -> AssemblyPlainFull.B.
Program Definition finish (a : assemblyinformation) (b : AssemblyPlainWritable.B) : (assemblyinformation * AssemblyPlainFull.B) :=
    ( id_assembly_full_ainfo_from_writable a
    , id_assembly_full_buffer_from_writable b ).

Axiom assembly_add_content : BufferPlain.buffer_t -> N -> N -> AssemblyPlainWritable.B -> N.
Program Definition backup (a : assemblyinformation) (b : AssemblyPlainWritable.B) (fpos : N) (content : BufferPlain.buffer_t) : (assemblyinformation * blockinformation) :=
    let apos_n := a.(apos) in
    let bsz := BufferPlain.buffer_len content in
    let chksum := calc_checksum content in
    let nwritten := assembly_add_content content bsz apos_n b in
    let bi := {| blockid   := 1
            ; bchecksum := chksum
            ; blocksize := nwritten
            ; filepos   := fpos
            ; blockaid  := aid a
            ; blockapos := apos_n |} in
    let a' := set_apos a (apos_n + nwritten) in
    (a', bi).

End Code_Writeable.


Section Code_Readable.
(** operations on AssemblyPlainFull *)

Axiom id_buffer_t_from_full : AssemblyPlainFull.B -> BufferPlain.buffer_t.
Axiom id_assembly_enc_buffer_t_from_buf : BufferEncrypted.buffer_t -> AssemblyEncrypted.B.
Program Definition encrypt (a : assemblyinformation) (b : AssemblyPlainFull.B) (ki : keyinformation) : option (assemblyinformation * AssemblyEncrypted.B) :=
    let a' := set_apos a (assemblysize (nchunks a)) in
    let benc  := Cstdio.encrypt (id_buffer_t_from_full b) (ivec ki) (pkey ki) in
    let b' := id_assembly_enc_buffer_t_from_buf benc in
    Some (a', b').

Axiom assembly_get_content : AssemblyPlainFull.B -> N -> N -> BufferPlain.buffer_t -> N.
Program Definition restore (b : AssemblyPlainFull.B) (bi : blockinformation) : option BufferPlain.buffer_t :=
    let bsz := bi.(blocksize) in
    let b' := BufferPlain.buffer_create bsz in
    let nw := assembly_get_content b bsz bi.(blockapos) b' in
    if N.eqb nw bsz then
        let bcksum := calc_checksum b' in
        if String.eqb bcksum bi.(bchecksum) then
            Some b'
        else
            None
    else None.

End Code_Readable.


Section Code_Encrypted.
(** operations on AssemblyEncrypted *)

Axiom id_buffer_t_from_enc : AssemblyEncrypted.B -> BufferEncrypted.buffer_t.
Axiom id_assembly_plain_buffer_t_from_buf : BufferPlain.buffer_t -> AssemblyPlainFull.B.
Program Definition decrypt (a : assemblyinformation) (b : AssemblyEncrypted.B) (ki : keyinformation) : option (assemblyinformation * AssemblyPlainFull.B) :=
    let a' := set_apos a 0 in
    let bdec := Cstdio.decrypt (id_buffer_t_from_enc b) (ivec ki) (pkey ki) in
    let b' := id_assembly_plain_buffer_t_from_buf bdec in
    Some (a', b').

Axiom chunk_identifier : configuration -> aid_t -> positive -> string.
Axiom chunk_identifier_path : configuration -> aid_t -> positive -> string.
Local Program Definition load_chunk_from_path (sfp : string) : option BufferEncrypted.buffer_t :=
    if Filesystem.Path.file_exists (Filesystem.Path.from_string sfp)
    then
        match Cstdio.fopen sfp Cstdio.read_mode with
        | None => None
        | Some fptr =>
            match Cstdio.fread fptr (chunksize_N) with
            | None => let _ := Cstdio.fclose fptr in None
            | Some (cnt, b) => match Cstdio.fclose fptr with
                | None => None
                | Some unit =>
                    if N.eqb cnt chunksize_N
                    then
                        Some (BufferEncrypted.from_buffer b)
                    else
                        None
                end
            end
        end
    else
        None.


Axiom id_enc_from_buffer_t : BufferEncrypted.buffer_t -> AssemblyEncrypted.B.
Program Definition recall (c : configuration) (a : assemblyinformation) : option (assemblyinformation * AssemblyEncrypted.B) :=
    let cidlist := Utilities.make_list (nchunks a) in
    let b := BufferEncrypted.buffer_create (Conversion.pos2N (nchunks a) * chunksize_N) in
    let aid := aid a in
    let blen := BufferEncrypted.buffer_len b in
    let nread := List.fold_left
                    (fun nread cid =>
                        let cpath := chunk_identifier_path c aid cid in
                        match load_chunk_from_path cpath with
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
    let a' := set_apos a nread in
    let b' := id_enc_from_buffer_t b in
    if N.eqb nread blen
    then Some (a', b')
    else None.

Local Program Definition store_chunk_to_path (sfp : string) (sz : N) (pos : N) (b : BufferEncrypted.buffer_t) : N :=
    let fp := Filesystem.Path.from_string sfp in
    if Filesystem.Path.file_exists fp
    then
        0
    else
        let dir := Filesystem.Path.parent fp in
        if orb (Filesystem.Path.is_directory dir) (Filesystem.create_directories dir) then
            match Cstdio.fopen sfp Cstdio.write_new_mode with
            | None => 0
            | Some fptr =>
                let buf := Cstdio.BufferEncrypted.buffer_create sz in
                if N.ltb 0 (Cstdio.BufferEncrypted.copy_sz_pos b pos sz buf 0)
                then
                    let res := match Cstdio.fwrite fptr sz (Cstdio.BufferEncrypted.to_buffer buf) with
                               | None => 0
                               | Some cnt => cnt
                               end
                    in
                    match Cstdio.fflush fptr with
                    | None => res
                    | Some fptr2 => match Cstdio.fclose fptr2 with
                                 | None => 0
                                 | Some unit => res
                                 end
                    end
                else
                    match Cstdio.fclose fptr with
                    | None => 0
                    | Some unit => (2 - 1 - 1)
                    end
            end
        else
            0.

Program Definition extract (c : configuration) (a : assemblyinformation) (b : AssemblyEncrypted.B) : N :=
    let aid := aid a in
    let buf := id_buffer_t_from_enc b in
    List.fold_left
        ( fun nwritten cid =>
            let cpath := chunk_identifier_path c aid cid in
            let apos := chunksize_N * ((Conversion.pos2N cid) - 1) in
            nwritten + store_chunk_to_path cpath chunksize_N apos buf
        )
        (Utilities.make_list (nchunks a))
        0.

End Code_Encrypted.

End Assembly.
