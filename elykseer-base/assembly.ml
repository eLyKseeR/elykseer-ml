open Mlcpp_cstdio

type b = Cstdio.File.Buffer.ta

exception Content_error of string

(* error codes of assembly.cxx *)
let describe = function
  | -1 -> "source buffer too small"
  | -2 -> "target buffer too small"
  | -3 -> "size < 1"
  | -4 -> "negative position"
  | -5 -> "assembly size not a multiple of the chunk size"
  | _ -> "unknown error"

let check op sz pos res =
  if res < 0 then
    raise (Content_error (Printf.sprintf "%s of %d bytes at %d failed: %s (%d)" op sz pos (describe res) res))
  else res

external cpp_add_content : b -> int -> int -> b -> int = "cpp_add_content"
let add_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt =
  cpp_add_content src sz pos tgt |> check "add_content" sz pos

external cpp_get_content : b -> int -> int -> b -> int = "cpp_get_content"
let get_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt =
  cpp_get_content src sz pos tgt |> check "get_content" sz pos
