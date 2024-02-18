(**
      e L y K s e e R
*)

From Coq Require Import Strings.String.
Require Import NArith.

Module Export Filesystem.

(**
 Module: Filesystem
 Description: interface to C++ <filesystem> functions
 *)

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.
 
Open Scope N_scope.
Open Scope string_scope.

Axiom path : Type.

Module Path.

    Axiom to_string : path -> string.
    Axiom from_string : string -> path.

    Axiom append : path -> path -> path.

    Axiom temp_directory : unit -> path.

    Axiom file_exists : path -> bool.
    
    Axiom file_size : path -> N.

    Axiom filename : path -> path.
    Axiom extension : path -> path.
    Axiom parent : path -> path.
    Axiom root : path -> path.
    
    Axiom absolute : path -> option path.
    Axiom relative : path -> option path.
    Axiom proximate : path -> option path.
    Axiom canonical : path -> option path.
    Axiom weakly_canonical : path -> option path.

    Axiom path_type : path -> string.
    Axiom is_regular_file : path -> bool.
    Axiom is_directory : path -> bool.
    Axiom is_fifo : path -> bool.
    Axiom is_block_file : path -> bool.
    Axiom is_character_file : path -> bool.
    Axiom is_socket : path -> bool.
    Axiom is_symlink : path -> bool.
    Axiom is_other : path -> bool.

End Path.

Module Permissions.

    Axiom permissions : Type.

    Axiom get : path -> option permissions.
    Axiom set : path -> permissions -> bool.
    Axiom add : path -> permissions -> bool.
    Axiom remove : path -> permissions -> bool.
    Axiom to_string : permissions -> string.
    Axiom to_dec : permissions -> positive.
    Axiom to_oct : permissions -> positive.
    Axiom from_oct : positive -> permissions.

End Permissions.


Axiom get_cwd : unit -> path.
Axiom set_cwd : path -> bool.

Axiom copy : path -> path -> bool.
Axiom copy_file : path -> path -> bool.
Axiom copy_symlink : path -> path -> bool.

Axiom create_directory : path -> bool.
Axiom create_directories : path -> bool.

Axiom create_hard_link : path -> path -> bool.
Axiom create_symlink : path -> path -> bool.
Axiom create_directory_symlink : path -> path -> bool.

Axiom equivalent : path -> path -> bool.

Axiom hard_link_count : path -> positive.

Axiom read_symlink : path -> option path.

Axiom remove : path -> bool.
Axiom remove_all : path -> positive.

Axiom rename : path -> path -> bool.

Axiom resize_file : path -> N -> bool.

Axiom space : path -> list N.

Axiom direntry : Type.

Module Direntry.

    Axiom as_path : direntry -> path.
    Definition as_string (de: direntry) : string := Filesystem.Path.to_string (Filesystem.Direntry.as_path de).

    Axiom direntry_exists : direntry -> bool.

    Axiom is_regular_file : direntry -> bool.
    Axiom is_block_file : direntry -> bool.
    Axiom is_character_file : direntry -> bool.
    Axiom is_directory : direntry -> bool.
    Axiom is_fifo : direntry -> bool.
    Axiom is_other : direntry -> bool.
    Axiom is_socket : direntry -> bool.
    Axiom is_symlink : direntry -> bool.

    Axiom file_size : direntry -> N.
    Axiom hard_link_count : direntry -> N.
    
End Direntry.

Axiom list_directory : forall A, path -> A -> (direntry -> A -> A) -> A.

End Filesystem.
