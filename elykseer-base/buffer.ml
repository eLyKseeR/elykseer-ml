open Mlcpp_cstdio

type b = Cstdio.File.Buffer.ta

let sha256 b =
  Elykseer_crypto.Sha256.buffer b
