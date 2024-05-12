open Mlcpp_cstdio

type b = Cstdio.File.Buffer.ta

external cpp_add_content : b -> int -> int -> b -> int = "cpp_add_content"
let add_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt =
  let res = cpp_add_content src sz pos tgt in
  if res < 0 then begin
      Printf.printf "add_content %d @ %d => %d\n" sz pos res; 0
    end
  else res

external cpp_get_content : b -> int -> int -> b -> int = "cpp_get_content"
let get_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt =
  let res = cpp_get_content src sz pos tgt in
  if res < 0 then begin
      Printf.printf "get_content %d @ %d => %d\n" sz pos res; 0
    end
  else res
