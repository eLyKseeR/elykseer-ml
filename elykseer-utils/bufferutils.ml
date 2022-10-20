open Mlcpp_cstdio

let add_string_to_buffer bin bpos s =
  let ls = String.length s in
  let bs = Cstdio.File.Buffer.from_string s in
  let lb = Cstdio.File.Buffer.size bin in
  Cstdio.File.Buffer.resize bin (ls + lb);
  Cstdio.File.Buffer.copy_sz_pos bs ~pos1:0 ~sz:ls bin ~pos2:bpos |> ignore;
  (bpos + ls, bin)
