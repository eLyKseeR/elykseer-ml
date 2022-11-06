
open Elykseer__Lxr

open Mlcpp_cstdio
open Mlcpp_filesystem

open Yojson.Basic.Util

let encode_el (aid,ki) bin bpos =
  let header = Printf.sprintf "{\"aid\":\"%s\",\"keys\":{" aid in
  let footer = "}}," in
  (* currently only one string: pkey *)
  let ks = Printf.sprintf "\"pkey\":\"%s\",\"localid\":%d,\"localnchunks\":%d"
     (RelationAidKey.pkey ki)
     (Conversion.n2i @@ RelationAidKey.localid ki)
     (Conversion.p2i @@ RelationAidKey.localnchunks ki) in
  Bufferutils.add_string_to_buffer bin bpos (header ^ ks ^ footer)

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
  encode_relation (RelationAidKey.M.elements rel) b' bpos'

let save_to_file rel path tgtuser =
  let (_bpos', b') = output_to_buffer rel in
  Fileutils.save_compressed_encrypted_file b' path tgtuser

let parse_keyrecord json : RelationAidKey.keyinformation =
  { pkey = json |> member "pkey" |> to_string
  ; localnchunks = json |> member "localnchunks" |> to_int |> Conversion.i2p
  ; localid = json |> member "localid" |> to_int |> Conversion.i2n
  }

let rec parse_keys rel j =
  match j with
  | [] -> rel
  | json :: r ->
    let aid = json |> member "aid" |> to_string in
    let ki = json |> member "keys" |> parse_keyrecord in
    let rel' = RelationAidKey.add aid ki rel in
    parse_keys rel' r

let parse_buffer b =
  let j = Cstdio.File.Buffer.to_string b in
  let idx0 = String.index j '\000' in
  let j' = String.sub j 0 (idx0) in
  let rel = RelationAidKey.coq_new in
  let rel' = Yojson.Basic.from_string j' |> to_list |> parse_keys rel in
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