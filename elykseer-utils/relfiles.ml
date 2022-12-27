
open Elykseer__Lxr

open Lwt.Syntax

module Git_store = Irmin_git_unix.FS.KV(Irmin.Contents.Json_value)
module Git_info = Irmin_unix.Info(Git_store.Info)

type t = Git_store.t

type relation = {
  rfi : Filetypes.fileinformation;
  rfbs : Assembly.blockinformation list
}

type filehash = string

let my_id = ref "unset"
let my_log = ref "unset (0.0.1)"

let new_map (config : Configuration.configuration) =
  my_id := Printf.sprintf "%d" (Conversion.n2i config.my_id);
  my_log := Printf.sprintf "%d (%s)" (Conversion.n2i config.my_id) Version.version;
  let git_config = Irmin_git.config ~bare:true config.path_db in
  let* repo = Git_store.Repo.v git_config in
  Git_store.main repo

let repo_path fhash =
  let d1 = String.sub fhash 4 2 in
  [!my_id;"relfiles";d1;fhash]

let version_obj : Git_store.contents =
  `O[ "major", `String Version.major
    ; "minor", `String Version.minor
    ; "build", `String Version.build ]

let fi2json_v1 (fi : Filetypes.fileinformation) : Git_store.contents =
  `O [ "fname", `String fi.fname
     ; "fsize", `String (string_of_int (Conversion.n2i fi.fsize))
     ; "fowner", `String fi.fowner
     ; "fpermissions", `String (string_of_int (Conversion.n2i fi.fpermissions))
     ; "fmodified", `String fi.fmodified
     ; "fchecksum", `String fi.fchecksum
  ]

let block2json_v1 (fb : Assembly.blockinformation) : Git_store.contents =
  `O [ "blockid", `String (string_of_int (Conversion.p2i fb.blockid))
     ; "bchecksum", `String fb.bchecksum
     ; "blocksize", `String (string_of_int (Conversion.n2i fb.blocksize))
     ; "filepos", `String (string_of_int (Conversion.n2i fb.filepos))
     ; "blockaid", `String fb.blockaid
     ; "blockapos", `String (string_of_int (Conversion.n2i fb.blockapos)) ]
let rel2json_v1 (rel : relation) : Git_store.contents =
  `O [ "version", version_obj
     ; "fileinformation", fi2json_v1 rel.rfi
     ; "blocks", `A (List.fold_left (fun acc fb -> block2json_v1 fb :: acc) [] (rel.rfbs)) ]

let msg_info msg = Git_info.v ~author:!my_log "%s" msg

(** add: sets fhash -> relation *)
let add fhash relation db =
  let msg = Fmt.str "update of %s" fhash in
  let reljson = rel2json_v1 relation in
  let%lwt () =
    try%lwt
      let fp = repo_path fhash in
      Git_store.set_exn ~info:(msg_info msg) db fp reljson
    with Failure e -> Lwt_io.eprintlf "error : %s" e in
  Lwt.return db

let json2fi_v1 fi : Filetypes.fileinformation =
    { fname = Relutils.get_str "fname" fi
    ; fsize = Relutils.get_int "fsize" fi |> Conversion.i2n
    ; fowner = Relutils.get_str "fowner" fi
    ; fpermissions = Relutils.get_int "fpermissions" fi |> Conversion.i2n
    ; fmodified = Relutils.get_str "fmodified" fi
    ; fchecksum = Relutils.get_str "fchecksum" fi
    }

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

let json2relation_versioned vmajor vfi varr =
  match vmajor with
  | 0
  | 1 -> let bs = json2blocks_v1 varr in
         let fi = json2fi_v1 vfi in
         Some { rfi = fi; rfbs = bs }
  | _ -> None

let json2blocks_opt (el : (string * Git_store.contents) list) =
  match el with
  | [] -> None
  | rs ->
    let vobj = Relutils.get_obj "version" rs in
    let vmajor = Relutils.get_int "major" vobj in
    let vfi = Relutils.get_obj "fileinformation" rs in
    let varr = Relutils.get_arr "blocks" rs in
    json2relation_versioned vmajor vfi varr

(** find: gets fhash -> relation option *)
let find fhash db =
  let fp = repo_path fhash in
  let%lwt res = try%lwt 
                  let%lwt res = Git_store.get db fp in Lwt.return (Some res)
                with _ -> Lwt.return None in
  Lwt.return @@ match res with
  | Some (`O el) -> json2blocks_opt el
  | _ -> None

let close_map db = Git_store.Repo.close (Git_store.repo db)