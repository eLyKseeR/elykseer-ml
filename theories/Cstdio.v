(**
      e L y K s e e R
*)

From Stdlib Require Import Strings.String.
From Stdlib Require Import NArith.

Module Export Cstdio.

(**
 Module: Cstdio
 Description: interface to C stdio functions and abstraction of C buffers
 *)

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Open Scope N_scope.
Open Scope string_scope.

Inductive EncryptionState := Plain | Encrypted.

Axiom cstdio_buffer : Type.

Section Mlcpp_Cstdio.

  Definition mode := string.
  Definition read_mode : mode := "rb".
  Definition write_mode : mode := "wb".
  Definition write_new_mode : mode := "wx".
  Definition append_mode : mode := "ab".

  Axiom fptr : Type.
  Axiom fopen : string -> mode -> option fptr.
  Axiom fclose : fptr -> option unit.
  Axiom fflush : fptr -> option fptr.
  Axiom fread : fptr -> N -> option (N * cstdio_buffer).
  Axiom fwrite : fptr -> N -> cstdio_buffer -> option N.
  Axiom ftell : fptr -> option N.
  Axiom fseek : fptr -> N -> option fptr.

End Mlcpp_Cstdio.

Module Type BUF.
  Axiom buffer_t : Type.
  Axiom buffer_create : N -> buffer_t.
  Axiom buffer_len : buffer_t -> N.
  Axiom calc_checksum : buffer_t -> string.
  Axiom copy_sz_pos : buffer_t -> N -> N -> buffer_t -> N -> N.
  Axiom from_buffer : cstdio_buffer -> buffer_t.
  Axiom to_buffer : buffer_t -> cstdio_buffer.

  Parameter state : EncryptionState.
End BUF.

Module Export BufferEncrypted : BUF.
  Axiom buffer_t : Type.
  Axiom buffer_create : N -> buffer_t.
  Axiom buffer_len : buffer_t -> N.
  Axiom calc_checksum : buffer_t -> string.
  Axiom copy_sz_pos : buffer_t -> N -> N -> buffer_t -> N -> N.
  Axiom from_buffer : cstdio_buffer -> buffer_t.
  Axiom to_buffer : buffer_t -> cstdio_buffer.

  Definition state := Encrypted.
End BufferEncrypted.
(* Print BufferEncrypted. *)

Module Export BufferPlain : BUF.
  Axiom buffer_t : Type.
  Axiom buffer_create : N -> buffer_t.
  Axiom buffer_len : buffer_t -> N.
  Axiom calc_checksum : buffer_t -> string.
  Axiom copy_sz_pos : buffer_t -> N -> N -> buffer_t -> N -> N.
  Axiom from_buffer : cstdio_buffer -> buffer_t.
  Axiom to_buffer : buffer_t -> cstdio_buffer.

  Definition state := Plain.
End BufferPlain.
(* Print BufferPlain. *)

(* AES-256-GCM authentication tag, stored in the last tag_len bytes of an assembly (physical
   position, i.e. the end of the last chunk) *)
Definition tag_len : N := 16.

(* AES-256-GCM: the nonce is derived from the ivec, the aad is authenticated but not encrypted *)
Axiom cpp_encrypt_buffer : BufferPlain.buffer_t -> N -> string -> string -> string -> option (N * BufferEncrypted.buffer_t).

(** Specification: AES-256-GCM encryption.
    When successful, returns the ciphertext size and encrypted buffer. *)
Axiom encrypt_spec : forall b sz aad iv pw n c,
  0 < sz -> sz <= BufferPlain.buffer_len b ->
  cpp_encrypt_buffer b sz aad iv pw = Some (n, c) ->
  n = sz /\ BufferEncrypted.buffer_len c = BufferPlain.buffer_len b.

Definition encrypt (bin : BufferPlain.buffer_t) (aad : string) (iv : string) (pw : string) : option (N * BufferEncrypted.buffer_t) :=
  let blen := BufferPlain.buffer_len bin in
  (* the data length is the buffer length minus the tag *)
  cpp_encrypt_buffer bin (blen - tag_len) aad iv pw.
(* returns None if the authentication tag does not verify *)
Axiom cpp_decrypt_buffer : BufferEncrypted.buffer_t -> string -> string -> string -> option (N * BufferPlain.buffer_t).

(** Specification: AES-256-GCM decryption.
    Succeeds only with correct aad, iv, and pw. Returns None on authentication failure. *)
Axiom decrypt_spec : forall c aad iv pw n b,
  cpp_decrypt_buffer c aad iv pw = Some (n, b) ->
  n <= BufferEncrypted.buffer_len c /\ BufferPlain.buffer_len b = BufferEncrypted.buffer_len c.

(** AEAD correctness: encrypt then decrypt with same parameters recovers the plaintext size. *)
Axiom aead_correct : forall b sz aad iv pw n c n' b',
  cpp_encrypt_buffer b sz aad iv pw = Some (n, c) ->
  cpp_decrypt_buffer c aad iv pw = Some (n', b') ->
  n' = n.

(** AEAD authentication: decrypt fails with wrong aad. *)
Axiom aead_wrong_aad : forall c aad aad' iv pw,
  aad <> aad' ->
  (exists b sz n ec, cpp_encrypt_buffer b sz aad iv pw = Some (n, ec) /\ c = ec) ->
  cpp_decrypt_buffer c aad' iv pw = None.

(** AEAD authentication: decrypt fails with wrong key. *)
Axiom aead_wrong_key : forall c aad iv pw pw',
  pw <> pw' ->
  (exists b sz n ec, cpp_encrypt_buffer b sz aad iv pw = Some (n, ec) /\ c = ec) ->
  cpp_decrypt_buffer c aad iv pw' = None.

(** AEAD authentication: decrypt fails with wrong iv. *)
Axiom aead_wrong_iv : forall c aad iv iv' pw,
  iv <> iv' ->
  (exists b sz n ec, cpp_encrypt_buffer b sz aad iv pw = Some (n, ec) /\ c = ec) ->
  cpp_decrypt_buffer c aad iv' pw = None.

Definition decrypt (bin : BufferEncrypted.buffer_t) (aad : string) (iv : string) (pw : string) : option (N * BufferPlain.buffer_t) :=
  cpp_decrypt_buffer bin aad iv pw.

Axiom cpp_ranbuf128 : unit -> cstdio_buffer.
Program Definition ranbuf128 (_ : unit) : BufferPlain.buffer_t :=
  let rb := cpp_ranbuf128 tt in
  BufferPlain.from_buffer rb.

End Cstdio.
