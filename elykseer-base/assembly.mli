
type b = Mlcpp_cstdio.Cstdio.File.Buffer.ta

(** raised when the C++ code rejects a copy (bad size, position, or buffer);
    the message names the operation, size, position, and error code *)
exception Content_error of string

(** copy _sz_ bytes from src buffer to target buffer at position _pos_
    target buffer is required to have size a multiple of 256*1024
    @raise Content_error if the copy is rejected
 *)
val add_content : src:b -> sz:int -> pos:int -> tgt:b -> int

(** copy _sz_ bytes from src buffer at position _pos_ to target buffer
    source buffer is required to have size a multiple of 256*1024
    @raise Content_error if the copy is rejected
 *)
val get_content : src:b -> sz:int -> pos:int -> tgt:b -> int
