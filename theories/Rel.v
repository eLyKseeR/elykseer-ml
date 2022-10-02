
(* this is the signature for relations *)

Module Type REL.
    Parameter key : Type.
    Parameter elt : Type.
    Parameter Map : Type.
    Parameter new : Map.
    Parameter find : key -> Map -> option elt.
    Parameter add : key -> elt -> Map -> Map.
End REL.
