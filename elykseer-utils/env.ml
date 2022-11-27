open Elykseer__Lxr

type pl = (string * Assembly.blockinformation) list
type t = (string * Assembly.blockinformation list) list

let consolidate_files (bis : pl) : t =
  let fnames = List.map (fst) bis |> List.sort_uniq (compare) in
  List.fold_left (
    fun acc fname ->
      let blocks = List.filter (fun fbi -> fname = (fst fbi)) bis |> List.map (snd) in
      (fname, blocks) :: acc
    ) [] fnames
