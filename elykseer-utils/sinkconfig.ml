open Elykseer__Lxr

let from_json_fs_sink c nm cs ac =
  match nm with
  | None -> None
  | Some name ->
    match Yojson.Basic.Util.member "user" cs |> Yojson.Basic.Util.to_string_option with
    | None -> None
    | Some user ->
      match Yojson.Basic.Util.member "basepath" ac |> Yojson.Basic.Util.to_string_option with
      | None -> None
      | Some basepath ->
        let smap = List.fold_left (fun acc (k,v) -> Distribution.SMap.add k v acc ) Distribution.SMap.empty  [("name", name); ("user", user); ("basepath", basepath)] in
        match Distribution.FSSink.init c smap with
        | None -> None
        | Some s -> Some (Distribution.FS s)

let from_json_s3_sink ~insecure c nm cs ac =
  match nm with
  | None -> None
  | Some name ->
    match Sinkcheck.resolve_secret cs "access-key" with
    | None -> Printf.eprintf "sink '%s': missing access-key (or access-key-env)\n" name; None
    | Some user ->
      match Sinkcheck.resolve_secret cs "secret-key" with
      | None -> Printf.eprintf "sink '%s': missing secret-key (or secret-key-env)\n" name; None
      | Some password ->
        match Yojson.Basic.Util.member "bucket" ac |> Yojson.Basic.Util.to_string_option with
        | None -> None
        | Some bucket ->
          let prefix = match Yojson.Basic.Util.member "prefix" ac |> Yojson.Basic.Util.to_string_option with
            | None -> ""
            | Some prefix -> prefix
            in
            match Yojson.Basic.Util.member "host" ac |> Yojson.Basic.Util.to_string_option with
            | None -> None
            | Some host ->
              match Yojson.Basic.Util.member "port" ac |> Yojson.Basic.Util.to_string_option with
              | None -> None
              | Some port ->
                match Yojson.Basic.Util.member "protocol" ac |> Yojson.Basic.Util.to_string_option with
                | None -> None
                | Some protocol ->
                  match Sinkcheck.check_protocol ~insecure ~host protocol with
                  | Error msg -> Printf.eprintf "sink '%s': %s\n" name msg; None
                  | Ok () ->
                  let smap = List.fold_left (fun acc (k,v) -> Distribution.SMap.add k v acc ) Distribution.SMap.empty  [("name", name); ("access", user); ("secret", password); ("bucket", bucket); ("prefix", prefix); ("protocol", protocol); ("host", host); ("port", port)] in
                  Distribution.S3Sink.init c smap

let from_json_sink ~insecure c s =
  let ty = Yojson.Basic.Util.member "type" s |> Yojson.Basic.Util.to_string_option
  and nm = Yojson.Basic.Util.member "name" s |> Yojson.Basic.Util.to_string_option
  and cs = Yojson.Basic.Util.member "credentials" s
  and ac = Yojson.Basic.Util.member "access" s in
  (* let () = Printf.printf "   sink: %s\n" (match nm with | None -> "??" | Some nm' -> nm') in *)
  match ty with
  | Some "FS" -> from_json_fs_sink c nm cs ac
  | Some "S3" -> begin
      match (from_json_s3_sink ~insecure c nm cs ac) with
      | Some s -> Some (Distribution.S3 s)
      | _ -> None
    end
  | _ -> None

let from_json_sinks ~insecure c j =
  match Yojson.Basic.Util.member "sinks" j with
  | `Null -> []
  | j' -> Yojson.Basic.Util.to_list j' |> List.map (from_json_sink ~insecure c)

let has_inline_secrets j =
  match Yojson.Basic.Util.member "sinks" j with
  | `List ss -> List.exists (fun s ->
                  let cs = Yojson.Basic.Util.member "credentials" s in
                  Sinkcheck.has_inline_secret cs "access-key" || Sinkcheck.has_inline_secret cs "secret-key") ss
  | _ -> false
