
open Mlcpp_cstdio.Cstdio.File.Buffer
open Mlcpp_filesystem

val load_compressed_encrypted_file : Filesystem.path -> string -> ta option
val save_compressed_encrypted_file : ta -> Filesystem.path -> string -> bool
