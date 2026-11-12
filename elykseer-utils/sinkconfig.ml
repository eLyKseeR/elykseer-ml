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

let rec nat_of_int n = if n <= 0 then O else S (nat_of_int (n - 1))
let rec int_of_nat = function O -> 0 | S n -> 1 + int_of_nat n

(* attempts are counted in unary (nat): keep them small *)
let max_max_attempts = 100

(* the optional "retry" object of a sink overrides the defaults of its type *)
let with_retry name s (p : Distribution.retrypolicy) : Distribution.retrypolicy option =
  match Yojson.Basic.Util.member "retry" s with
  | `Null -> Some p
  | `Assoc _ as r ->
    let field k = match Yojson.Basic.Util.member k r with
      | `Null -> Ok None
      | `Int i when i >= 0 && (k <> "max_attempts" || i <= max_max_attempts) -> Ok (Some i)
      | _ -> Error k in
    begin match field "max_attempts", field "base_delay_ms", field "max_delay_ms", field "deadline_ms" with
    | Ok a, Ok b, Ok m, Ok d ->
      let n o dflt = match o with Some i -> Conversion.i2n i | None -> dflt in
      Some { Distribution.max_attempts = (match a with Some i -> nat_of_int i | None -> p.max_attempts);
             base_delay_ms = n b p.base_delay_ms;
             max_delay_ms = n m p.max_delay_ms;
             deadline_ms = n d p.deadline_ms }
    | Error k, _, _, _ | _, Error k, _, _ | _, _, Error k, _ | _, _, _, Error k ->
      Printf.eprintf "sink '%s': retry.%s must be a non-negative integer%s\n" name k
        (if k = "max_attempts" then Printf.sprintf " up to %d" max_max_attempts else ""); None
    end
  | _ -> Printf.eprintf "sink '%s': retry must be an object\n" name; None

let set_retry s = function
  | Some (Distribution.FS x) ->
    Option.map (fun r -> Distribution.FS { x with retry = r }) (with_retry x.name s x.retry)
  | Some (Distribution.S3 x) ->
    Option.map (fun r -> Distribution.S3 { x with retry = r }) (with_retry x.name s x.retry)
  | None -> None

let from_json_sink ~insecure c s =
  let ty = Yojson.Basic.Util.member "type" s |> Yojson.Basic.Util.to_string_option
  and nm = Yojson.Basic.Util.member "name" s |> Yojson.Basic.Util.to_string_option
  and cs = Yojson.Basic.Util.member "credentials" s
  and ac = Yojson.Basic.Util.member "access" s in
  (* let () = Printf.printf "   sink: %s\n" (match nm with | None -> "??" | Some nm' -> nm') in *)
  set_retry s @@ match ty with
  | Some "FS" -> from_json_fs_sink c nm cs ac
  | Some "S3" -> Option.map (fun s -> Distribution.S3 s) (from_json_s3_sink ~insecure c nm cs ac)
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
