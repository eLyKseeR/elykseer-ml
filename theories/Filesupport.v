(**
      e L y K s e e R
*)

(**
 Module: Filesupport
 Description: provides abstract definitions of file functions.
 *)

 From Coq Require Import Strings.String.
 Require Import NArith.
 
 Module Export Filesupport.

Definition filename := string.

Record fileinformation : Type := mkfileinformation
     { fname : filename
     ; fsize : N
     ; fowner : string
     ; fpermissions : N
     ; fmodified : string
     ; fchecksum : string
     }.

Axiom get_file_information : filename -> fileinformation.

End Filesupport.
