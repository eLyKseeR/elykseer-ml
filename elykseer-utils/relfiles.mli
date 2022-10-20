open Elykseer__Lxr
open Mlcpp_filesystem
open Mlcpp_cstdio

val output_to_buffer : RelationFileAid.coq_Map -> int * Cstdio.File.Buffer.ta
val save_to_file : RelationFileAid.coq_Map -> Filesystem.path -> string -> bool
val load_from_file : Filesystem.path -> string -> RelationFileAid.coq_Map option
