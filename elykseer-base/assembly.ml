open Mlcpp_cstdio

type b = Cstdio.File.Buffer.ta

external cpp_add_content : b -> int -> int -> b -> int = "cpp_add_content"
let add_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt =
  cpp_add_content src sz pos tgt

external cpp_get_content : b -> int -> int -> b -> int = "cpp_get_content"
let get_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt =
  cpp_get_content src sz pos tgt

external cpp_b2s : b -> int -> int -> string = "cpp_b2s"
let b2s ~src:buf ~sz:sz ~pos:pos = cpp_b2s buf sz pos

external date_ident : unit -> string = "cpp_date_ident"
