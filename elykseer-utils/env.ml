open Elykseer__Lxr

type pl = (string * Assembly.blockinformation) list
type t = (string * Assembly.blockinformation list) list

(* groups the blocks by file in one pass (instead of filtering the whole
   list once per file); per file the blocks are sorted by file position,
   numbered from 1, and returned in reverse order; files in reverse order
   of their names *)
let consolidate_files (bis : pl) : t =
  let groups : (string, Assembly.blockinformation list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (fname, bi) ->
      let prev = Option.value ~default:[] (Hashtbl.find_opt groups fname) in
      Hashtbl.replace groups fname (bi :: prev)) bis;
  Hashtbl.fold (fun fname rblocks acc ->
      let blocks = List.rev rblocks |>
                   (* sort blocks by filepos ascending *)
                   List.stable_sort (fun (e1 : Assembly.blockinformation) e2 -> compare (Conversion.n2i e1.filepos) (Conversion.n2i e2.filepos)) |>
                   (* set increasing blockid *)
                   List.mapi (fun i0 (e : Assembly.blockinformation) -> {e with blockid = Conversion.i2p (i0 + 1)}) |>
                   (* reverse list *)
                   List.rev in
      (fname, blocks) :: acc) groups []
  |> List.sort (fun (f1, _) (f2, _) -> compare f2 f1)
