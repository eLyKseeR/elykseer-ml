(**
      e L y K s e e R
*)

(**
 Module: Filesupport
 Description: provides abstract definitions of file functions.
 *)

Module Export Filesupport.

From LXR Require Import Filetypes.

Axiom get_file_information : filename -> fileinformation.

End Filesupport.
