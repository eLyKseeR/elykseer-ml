let restore_relpath fname =
  let parts = String.split_on_char '/' fname
              |> List.filter (fun p -> p <> "" && p <> ".") in
  if List.mem ".." parts then
    Error (Printf.sprintf "file name '%s' contains '..'" fname)
  else if parts = [] then
    Error (Printf.sprintf "file name '%s' is empty" fname)
  else
    Ok (String.concat "/" parts)

let is_below ~base p =
  let base' = if String.ends_with ~suffix:"/" base then base else base ^ "/" in
  p = base || String.starts_with ~prefix:base' p
