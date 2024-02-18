open Mlcpp_cstdio

external cpp_buffer_id : 'a -> 'b = "cpp_buffer_id"
  
(** the chunk will be stored in a subdirectory
    which is the last two chars of the cid (cid[-2], cid[-1]),
    in a subdirectory (cid[-4], cid[-3])
*)
external mk_cid_subdir : string -> string = "cpp_mk_cid_subdir"

let ranbuf128 () =
  let r = Elykseer_crypto.Key128.mk () |> Elykseer_crypto.Key128.to_bytes in
  let b = Cstdio.File.Buffer.create (16) in
  Cstdio.File.Buffer.copy_string r b 16; b
