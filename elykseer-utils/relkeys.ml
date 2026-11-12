
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

(** add_all: sets aid -> keyinformation for all entries in one commit *)
let add_all kis db =
  let msg = Fmt.str "update of %d keys" (List.length kis) in
  try%lwt
    let%lwt () = Git_store.with_tree_exn ~info:(msg_info msg) db [] (fun t ->
      let t0 = Option.value t ~default:(Git_store.Tree.empty ()) in
      let%lwt t' = Lwt_list.fold_left_s (fun t (aid, ki) ->
                     Git_store.Tree.add t (repo_path aid) (keys2json_v1 ki)) t0 kis in
      Lwt.return (Some t')) in
    Lwt.return (Ok db)
  with e -> Lwt.return (Error (Printf.sprintf "cannot store %d keys: %s" (List.length kis) (Printexc.to_string e)))

(** add: sets aid -> keyinformation *)
let add aid keys0 db = add_all [(aid, keys0)] db

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
    (* a damaged entry reads as not found *)
    try
      let vobj = Relutils.get_obj "version" rs in
      let vmajor = Relutils.get_int "major" vobj in
      let vminor = Relutils.get_int "minor" vobj in
      let vbuild = Relutils.get_int "build" vobj in
      let vkeys = Relutils.get_obj "keys" rs in
      json2keys_versioned vmajor vminor vbuild vkeys
    with Failure _ | Not_found -> None

(** find_v: gets aid -> (version * keyinformation) option *)
let find_v aid db =
  let fp = repo_path aid in
  let%lwt res = Git_store.find db fp in
  Lwt.return @@ match res with
  | Some (`O el) -> json2keys_opt el
  | _ -> None

(** find: gets aid -> keyinformation option *)
let find aid db =
  Lwt.map (Option.map snd) (find_v aid db)

let close_map db = Git_store.Repo.close (Git_store.repo db)