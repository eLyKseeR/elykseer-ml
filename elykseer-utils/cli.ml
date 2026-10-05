let default_myid = "1234567890"

open Elykseer__Lxr

(* Nchunks.from_int clamps to its range, so the bounds are its extremes *)
let min_nchunks = Nchunks.from_int 1 |> Nchunks.to_positive |> Conversion.p2i
let max_nchunks = Nchunks.from_int max_int |> Nchunks.to_positive |> Conversion.p2i

let nchunks_spec r =
  Arg.Int (fun n ->
      if n < min_nchunks || n > max_nchunks then
        raise (Arg.Bad (Printf.sprintf "-n must be between %d and %d, got %d" min_nchunks max_nchunks n));
      r := n)

let usage_error argspec usage msg =
  Printf.eprintf "%s\n" msg;
  prerr_string (Arg.usage_string argspec usage);
  exit 2

let require argspec usage opts =
  match List.filter (fun (_, v) -> v = "") opts with
  | [] -> ()
  | missing ->
    usage_error argspec usage
      (Printf.sprintf "missing required option%s: %s"
         (if List.length missing > 1 then "s" else "")
         (String.concat ", " (List.map fst missing)))

let db_exists dbpath =
  Sys.file_exists dbpath && Sys.is_directory dbpath
  && Sys.file_exists (Filename.concat dbpath ".git")

let require_db dbpath =
  if not (db_exists dbpath) then begin
    Printf.eprintf "no meta data database found at '%s'\n" dbpath;
    exit 2
  end

let note_new_db dbpath =
  if not (db_exists dbpath) then
    Printf.eprintf "creating a new meta data database at '%s'\n%!" dbpath

let warn_default_myid myid =
  if myid = default_myid then
    Printf.eprintf "warning: no identifier given (-i), using the default '%s'\n%!" default_myid
