
open Elykseer__Lxr

open Lwt.Syntax

module Git_store = Irmin_git_unix.FS.KV(Irmin.Contents.Json_value)
module Git_info = Irmin_unix.Info(Git_store.Info)

type t = Git_store.t

let my_id = ref "unset"
let my_log = ref "unset (0.0.1)"

let new_map (config : Configuration.configuration) =
  my_id := Printf.sprintf "%d" (Conversion.n2i config.my_id);
  my_log := Printf.sprintf "%d (%s)" (Conversion.n2i config.my_id) Version.version;
  let git_config = Irmin_git.config ~bare:true config.path_db in
  let* repo = Git_store.Repo.v git_config in
  Git_store.main repo

let mk_repo_path fhash =
  let d1 = String.sub fhash 4 2 in
  [!my_id;d1;fhash]

let version_obj : Git_store.contents =
  `O[ "major", `String Version.major
    ; "minor", `String Version.minor
    ; "build", `String Version.build ]

let block2json_v1 (fb : Assembly.blockinformation) : Git_store.contents =
  `O [ ("blockid", `String (string_of_int (Conversion.p2i fb.blockid)))
     ; ("bchecksum", `String fb.bchecksum)
     ; ("blocksize", `String (string_of_int (Conversion.n2i fb.blocksize)))
     ; ("filepos", `String (string_of_int (Conversion.n2i fb.filepos)))
     ; ("blockaid", `String fb.blockaid)
     ; ("blockapos", `String (string_of_int (Conversion.n2i fb.blockapos))) ]
let blocks2json_v1 (fblocks : Assembly.blockinformation list) : Git_store.contents =
  `O [ "version", version_obj
     ; "blocks", `A (List.fold_left (fun acc fb -> block2json_v1 fb :: acc) [] fblocks) ]

let msg_info msg = Git_info.v ~author:!my_log "%s" msg

(** add: sets fhash -> [blockinformation] *)
let add fhash fblocks0 db =
  let msg = Fmt.str "update of %s" fhash in
  let fblocks = blocks2json_v1 fblocks0 in
  let%lwt () =
    try%lwt
      let fp = mk_repo_path fhash in
      Git_store.set_exn ~info:(msg_info msg) db fp fblocks
    with Failure e -> Lwt_io.eprintlf "error : %s" e in
  Lwt.return db

let json2block_v1 obs : Assembly.blockinformation option =
  match obs with
  | `O bs -> Some
    { blockid = Relutils.get_int "blockid" bs |> Conversion.i2p
    ; bchecksum = Relutils.get_str "bchecksum" bs
    ; blocksize = Relutils.get_int "blocksize" bs |> Conversion.i2n
    ; filepos = Relutils.get_int "filepos" bs |> Conversion.i2n
    ; blockaid = Relutils.get_str "blockaid" bs
    ; blockapos = Relutils.get_int "blockapos" bs |> Conversion.i2n
    }
  | _ -> None
let json2blocks_v1 blocks =
  match blocks with
  | [] -> []
  | bs -> List.fold_left (fun acc b -> match json2block_v1 b with Some e -> e :: acc | None -> acc) [] bs

let json2blocks_versioned vmajor varr =
  match vmajor with
  | 0
  | 1 -> begin
    match varr with
    | [] -> None
    | blocks -> Some (json2blocks_v1 blocks)
    end
  | _ -> None

let json2blocks_opt (el : (string * Git_store.contents) list) =
  match el with
  | [] -> None
  | rs ->
    let vobj = Relutils.get_obj "version" rs in
    let vmajor = Relutils.get_int "major" vobj in
    let varr = Relutils.get_arr "blocks" rs in
    json2blocks_versioned vmajor varr

(** find: gets fhash -> [blockinformation] option *)
let find fhash db =
  let fp = mk_repo_path fhash in
  let%lwt res = Git_store.get db fp in
  Lwt.return @@ match res with
  (* unversioned case *)
  | `A bs -> Some (json2blocks_v1 bs)
  | `O el -> json2blocks_opt el
  | _ -> None

let close_map db = Git_store.Repo.close (Git_store.repo db)