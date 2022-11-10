(**
      e L y K s e e R
*)

Module Export Buffer.

(**
 Module: Buffer
 Description: data buffer of specified length
 *)

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.
 
From Coq Require Import Strings.String.
Require Import NArith.
Open Scope N_scope.

From LXR Require Import Conversion.

Inductive EncryptionState := Plain | Encrypted.

Axiom cstdio_buffer : Type.

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

Axiom cpp_encrypt_buffer : BufferPlain.buffer_t -> string -> BufferEncrypted.buffer_t.
Definition encrypt (bin : BufferPlain.buffer_t) (pw : string) : BufferEncrypted.buffer_t :=
  cpp_encrypt_buffer bin pw.
Axiom cpp_decrypt_buffer : BufferEncrypted.buffer_t -> string -> BufferPlain.buffer_t.
Definition decrypt (bin : BufferEncrypted.buffer_t) (pw : string) : BufferPlain.buffer_t :=
  cpp_decrypt_buffer bin pw.

End Buffer.