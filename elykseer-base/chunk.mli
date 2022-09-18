
type t = Mlcpp_cstdio.Cstdio.File.Buffer.ta

val load_from_path : string -> t option
(* val store_to_path : t -> string *)

val get : t -> int -> char
val set : t -> int -> char -> unit
val size : t -> int
