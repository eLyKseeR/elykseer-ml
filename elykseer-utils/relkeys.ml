
open Elykseer__Lxr

open Lwt.Syntax

module Git_store = Irmin_git_unix.FS.KV(Irmin.Contents.Json_value)
module Git_info = Irmin_unix.Info(Git_store.Info)

type t = Git_store.t

type aid = string

let my_id = ref "unset"
let my_log = ref "unset (0.0.1)"

let new_map (config : Configuration.configuration) =
  my_id := Printf.sprintf "%s" config.my_id;
  my_log := Printf.sprintf "%s (%s)" config.my_id Version.version;
  let git_config = Irmin_git.config ~bare:true config.path_db in
  let* repo = Git_store.Repo.v git_config in
  Git_store.main repo


let repo_path aid =
  let d1 = String.sub aid 4 2 in
  [!my_id;"relkeys";d1;aid]

let version_obj : Git_store.contents =
  `O [ "major", `String Version.major
     ; "minor", `String Version.minor
     ; "build", `String Version.build ]

let key2json_v1 (k : Assembly.keyinformation) : Git_store.contents =
  `O [ ("ivec", `String k.ivec)
     ; ("pkey", `String k.pkey)
     ; ("localnchunks", `String (string_of_int (Conversion.p2i k.localnchunks))) ]
let keys2json_v1 (keys : Assembly.keyinformation) : Git_store.contents =
  `O [ "version", version_obj
     ; "keys", key2json_v1 keys ]

let msg_info msg = Git_info.v ~author:!my_log "%s" msg

(** add: sets aid -> keyinfornation *)
let add aid keys0 db =
  let msg = Fmt.str "update for %s" aid in
  let keys = keys2json_v1 keys0 in
  let%lwt () =
    try%lwt
      let fp = repo_path aid in
      Git_store.set_exn ~info:(msg_info msg) db fp keys
    with Failure e -> Lwt_io.eprintlf "error : %s" e in
  Lwt.return db

let json2keys_v1 version obs : (string * Assembly.keyinformation) option =
  match obs with
  | [] -> None
  | bs -> Some (version,
    { ivec = Relutils.get_str "ivec" bs
    ; pkey = Relutils.get_str "pkey" bs
    ; localnchunks = Relutils.get_int "localnchunks" bs |> Conversion.i2p
    })

let json2keys_versioned vmajor vminor vbuild vkeys =
  let version = Printf.sprintf "%d.%d.%d" vmajor vminor vbuild in
  match vmajor with
  | 0
  | 1 -> json2keys_v1 version vkeys
  | _ -> None

let json2keys_opt el =
  match el with
  | [] -> None
  | rs ->
    let vobj = Relutils.get_obj "version" rs in
    let vmajor = Relutils.get_int "major" vobj in
    let vminor = Relutils.get_int "minor" vobj in
    let vbuild = Relutils.get_int "build" vobj in
    let vkeys = Relutils.get_obj "keys" rs in
    json2keys_versioned vmajor vminor vbuild vkeys

(** find: gets aid -> keyinformation option *)
let find aid db =
  let fp = repo_path aid in
  let%lwt res = Git_store.get db fp in
  Lwt.return @@ match res with
  | `O el -> begin match json2keys_opt el with
               | Some (_,ki) -> Some ki
               | None -> None
             end
  | _ -> None

  (** find_v: gets aid -> (version * keyinformation) option *)
let find_v aid db =
  let fp = repo_path aid in
  let%lwt res = Git_store.get db fp in
  Lwt.return @@ match res with
  | `O el -> json2keys_opt el
  | _ -> None

let close_map db = Git_store.Repo.close (Git_store.repo db)