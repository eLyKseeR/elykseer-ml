
type b = Mlcpp_cstdio.Cstdio.File.Buffer.ta

(** copy _sz_ bytes from src buffer to target buffer at position _pos_
    target buffer is required to have size a multiple of 256*1024
 *)
val add_content : src:b -> sz:int -> pos:int -> tgt:b -> int

(** copy _sz_ bytes from src buffer at position _pos_ to target buffer
    source buffer is required to have size a multiple of 256*1024
 *)
val get_content : src:b -> sz:int -> pos:int -> tgt:b -> int

val b2s : src:b -> sz:int -> pos:int -> string

val date_ident : unit -> string
