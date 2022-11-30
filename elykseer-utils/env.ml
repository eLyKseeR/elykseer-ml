open Elykseer__Lxr

type pl = (string * Assembly.blockinformation) list
type t = (string * Assembly.blockinformation list) list

let consolidate_files (bis : pl) : t =
  let fnames = List.map (fst) bis |> List.sort_uniq (compare) in
  List.fold_left (
    fun acc fname ->
                   (* list only blocks in this file *)
      let blocks = List.filter (fun fbi -> fname = (fst fbi)) bis |> List.map (snd) |>
                   (* sort blocks by filepos ascending *)
                   List.sort (fun e1 e2 -> compare (Conversion.n2i (Assembly.filepos e1)) (Conversion.n2i (Assembly.filepos e2))) |>
                   (* set increasing blockid *)
                   List.mapi (fun i0 (e : Assembly.blockinformation) -> {e with blockid = Conversion.i2p (i0 + 1)}) |>
                   (* reverse list *)
                   List.rev in
      (fname, blocks) :: acc
    ) [] fnames
