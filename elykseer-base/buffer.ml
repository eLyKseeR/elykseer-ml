open Mlcpp_cstdio

type b = Cstdio.File.Buffer.ta

external buffer_sha_id : b -> Sha256.buf = "cpp_buffer_sha_id"
let sha256 b =
  let b' = buffer_sha_id b in
  Sha256.buffer b'
