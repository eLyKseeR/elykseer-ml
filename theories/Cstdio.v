(**
      e L y K s e e R
*)

From Coq Require Import Strings.String.
Require Import NArith.

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

Axiom cpp_encrypt_buffer : BufferPlain.buffer_t -> string -> string -> N * BufferEncrypted.buffer_t.
Definition encrypt (bin : BufferPlain.buffer_t) (iv : string) (pw : string) : (N * BufferEncrypted.buffer_t) :=
  cpp_encrypt_buffer bin iv pw.
Axiom cpp_decrypt_buffer : BufferEncrypted.buffer_t -> string -> string -> N * BufferPlain.buffer_t.
Definition decrypt (bin : BufferEncrypted.buffer_t) (iv : string) (pw : string) : (N * BufferPlain.buffer_t) :=
  cpp_decrypt_buffer bin iv pw.

Axiom cpp_ranbuf128 : unit -> cstdio_buffer.
Program Definition ranbuf128 (_ : unit) : BufferPlain.buffer_t :=
  let rb := cpp_ranbuf128 tt in
  BufferPlain.from_buffer rb.

End Cstdio.