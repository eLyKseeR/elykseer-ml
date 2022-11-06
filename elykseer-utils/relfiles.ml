
open Elykseer__Lxr

open Mlcpp_cstdio
open Mlcpp_filesystem

open Yojson.Basic.Util

let rec encode_blocks blocks bin bpos =
  match blocks with
  | [] ->
    let footer = "]," in
    (* bpos - 1 :  to correct for last ',' *)
    let (bpos', b') = Bufferutils.add_string_to_buffer bin (bpos - 1) footer in
    (bpos', b')
  | b :: r ->
    let s = Printf.sprintf "{\"fpos\":%d,\"sz\":%d,\"apos\":%d,\"chksum\":\"%s\"},"
              (Conversion.n2i @@ RelationFileAid.filepos b)
              (Conversion.n2i @@ RelationFileAid.blocksize b)
              (Conversion.n2i @@ RelationFileAid.blockapos b)
              (RelationFileAid.bchecksum b) in
    let (bpos', b') = Bufferutils.add_string_to_buffer bin bpos s in
    encode_blocks r b' bpos'


let rec encode_by_assembly assemblies blocks bin bpos =
  match assemblies with
  | [] -> (bpos, bin)
  | a :: r ->
    let ablocks = List.filter (fun b -> RelationFileAid.blockaid b == a) blocks in
    let header = Printf.sprintf "{\"aid\":\"%s\",\"blocks\":[" a in
    let (bpos', b') = Bufferutils.add_string_to_buffer bin bpos header in
    let (bpos'', b'') = encode_blocks ablocks b' bpos' in
    let footer = "}," in
    let (bpos''', b''') = Bufferutils.add_string_to_buffer b'' (bpos'' - 1) footer in
    encode_by_assembly r blocks b''' bpos'''


let encode_el (fpath,blocks) bin bpos =
  let header = Printf.sprintf "{\"path\":\"%s\",\"assemblies\":[" fpath in
  let footer = "]}," in
  let assemblies = List.map (fun b -> RelationFileAid.blockaid b) blocks |> List.sort_uniq (compare) in
  let (bpos', b') = Bufferutils.add_string_to_buffer bin bpos header in
  let (bpos'', b'') = encode_by_assembly assemblies blocks b' bpos' in
  Bufferutils.add_string_to_buffer b'' (bpos'' - 1) footer

(** 
type blockinformation_out = { blockid : positive; bchecksum : string;
                              blocksize : n; filepos : n; blockaid : string;
                              blockapos : n }

type assemblyinformation = { aid : string; blocks : blockinformation2 list }
type blockinformation = { bchecksum : string; blocksize : int; filepos : int; blockapos : int }

json (pretty printed):
  { "files": [
      { "path": "fname1",
        "assemblies": [
          { "aid": "aid00001",
            "blocks": [
              { "fpos": 0,
                "sz": 1303,
                "apos": 0,
                "chksum": "9459ab301ffeda0123456789"
              },
              { "fpos": 1303,
                "sz": 457,
                "apos": 1303,
                "chksum": "f7698e10adf104323ba95495"
              }
            ]
          }
        ]
      }
    ]
  }
*)

let rec encode_relation kvpairs bin bpos =
  match kvpairs with
  | [] ->
    Bufferutils.add_string_to_buffer bin (bpos - 1) "]"
  | el :: r ->
    let (bpos', b') = encode_el el bin bpos in
    encode_relation r b' bpos'

let output_to_buffer rel =
  let b0 = Cstdio.File.Buffer.create (64*1024) in
  let (bpos', b') = Bufferutils.add_string_to_buffer b0 0 "[" in
  encode_relation (RelationFileAid.M.elements rel) b' bpos'

let save_to_file rel path tgtuser =
  let (_bpos', b') = output_to_buffer rel in
  Fileutils.save_compressed_encrypted_file b' path tgtuser

let rec parse_blocks bid aid acc j =
  match j with
  | [] -> acc
  | json :: r ->
    let b : RelationFileAid.blockinformation =
      { blockid = Conversion.i2p bid
      ; filepos = json |> member "fpos" |> to_int |> Conversion.i2n
      ; bchecksum = json |> member "chksum" |> to_string
      ; blocksize = json |> member "sz" |> to_int |> Conversion.i2n
      ; blockapos = json |> member "apos" |> to_int |> Conversion.i2n
      ; blockaid = aid
      } in
    parse_blocks (bid + 1) aid (b :: acc) r
let rec parse_assemblies acc j =
  match j with
  | [] -> List.concat acc
  | json :: r ->
    let aid = json |> member "aid" |> to_string in
    let bs = json |> member "blocks" |> to_list |> parse_blocks 1 aid [] in
    parse_assemblies (bs :: acc) r

let rec parse_files rel j =
  match j with
  | [] -> rel
  | json :: r ->
    let fp = json |> member "path" |> to_string in
    let _as = json |> member "assemblies" |> to_list |> parse_assemblies [] in
    let rel' = RelationFileAid.add fp _as rel in
    parse_files rel' r

let parse_buffer b =
  let j = Cstdio.File.Buffer.to_string b in
  let idx0 = String.index j '\000' in
  let j' = String.sub j 0 (idx0) in
  let rel = RelationFileAid.coq_new in
  let rel' = Yojson.Basic.from_string j' |> to_list |> parse_files rel in
  Some rel'

let load_from_file path tgtuser =
  let path' = Filesystem.Path.to_string path in
  if Filesystem.Path.exists path
    then begin
      Fileutils.load_compressed_encrypted_file path tgtuser |> function
      | None -> Printf.printf "failed to load from file %s\n" path'; None
      | Some b -> parse_buffer b
      end
    else begin
      Printf.printf "relation file %s not found!" path'; None
      end