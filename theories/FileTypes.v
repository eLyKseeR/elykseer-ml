(**
      e L y K s e e R
*)

(**
 Module: Filetypes
 Description: provides abstract definitions of file functions.
 *)

Module Export Filetypes.

From Coq Require Import Strings.String.
Require Import NArith.

Definition filename := string.

Record fileinformation : Type := mkfileinformation
     { fname : filename
     ; fsize : N
     ; fowner : string
     ; fpermissions : N
     ; fmodified : string
     ; fchecksum : string
     }.
 
End Filetypes.
