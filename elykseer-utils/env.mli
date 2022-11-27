open Elykseer__Lxr

type pl = (string * Assembly.blockinformation) list
type t = (string * Assembly.blockinformation list) list

val consolidate_files :  pl -> t
