(**
      e L y K s e e R
*)

(**
 Module: FileSupport
 Description: provides abstract definitions of file functions.
 *)

Module Export FileTypes.

From Coq Require Import Strings.String Strings.Byte.
Open Scope string_scope.

Require Import NArith.
From Coq Require Import NArith.BinNat.
Open Scope N_scope.

Definition filename := string.
Definition fptr := nat.
Definition filelist := list filename.
Record fileinformation : Type := mkfileinformation
     { fname : filename
     ; fsize : N
     ; fowner : string
     ; fpermissions : N
     ; fmodified : string
     ; fchecksum : string
     }.
 
End FileTypes.
