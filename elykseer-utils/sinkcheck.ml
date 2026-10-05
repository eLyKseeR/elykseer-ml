let is_local_host host =
  match String.lowercase_ascii host with
  | "localhost" | "::1" | "[::1]" -> true
  | h -> String.starts_with ~prefix:"127." h

let check_protocol ~insecure ~host protocol =
  match String.lowercase_ascii protocol with
  | "https" -> Ok ()
  | "http" when insecure || is_local_host host -> Ok ()
  | "http" -> Error (Printf.sprintf "refusing to send credentials in clear over http to '%s' (use https, or --insecure)" host)
  | p -> Error (Printf.sprintf "unsupported protocol '%s'" p)

let member_string j key =
  match j with
  | `Assoc _ -> Yojson.Basic.Util.member key j |> Yojson.Basic.Util.to_string_option
  | _ -> None

let resolve_secret ?(getenv = Sys.getenv_opt) j key =
  match member_string j (key ^ "-env") with
  | Some var -> getenv var
  | None -> member_string j key

let has_inline_secret j key =
  member_string j (key ^ "-env") = None && member_string j key <> None

let check_config_mode ~insecure ~inline_secrets fp =
  match Unix.stat fp with
  | exception Unix.Unix_error (e, _, _) ->
    Error (Printf.sprintf "cannot access '%s': %s" fp (Unix.error_message e))
  | st ->
    if inline_secrets && not insecure && st.Unix.st_perm land 0o077 <> 0 then
      Error (Printf.sprintf "'%s' contains secrets but is accessible by group or others (mode %o); run: chmod 600 %s" fp st.Unix.st_perm fp)
    else Ok ()
