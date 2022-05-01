(**
      e L y K s e e R
*)

(**
 Module: FileSupport
 Description: provides abstract definitions of file functions.
 *)

Module Export FileSupport.

From LXR Require Import FileTypes.

Axiom get_file_information : filename -> fileinformation.

End FileSupport.
