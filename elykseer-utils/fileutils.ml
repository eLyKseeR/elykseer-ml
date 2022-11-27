open Mlcpp_filesystem
open Mlcpp_cstdio

(* TODO *)
let unencrypt bin tgtuser =
  Printf.printf "decrypt for %s\n" tgtuser;
  Some bin

(* TODO *)
let uncompress bin =
  Printf.printf "uncompress\n";
  bin

let rec load_file_parts path bin bpos =
  let path' = Filesystem.Path.to_string path in
  match Cstdio.File.content64k path' bpos with
  | Ok b ->
    let bsz = Cstdio.File.Buffer.size b in
    let binsz = Cstdio.File.Buffer.size bin in
    Cstdio.File.Buffer.resize bin (bsz + binsz);
    Cstdio.File.Buffer.copy_sz_pos b ~pos1:0 ~sz:bsz bin ~pos2:bpos |> ignore;
    load_file_parts path bin (bpos + bsz)
  | Error _ -> (bpos, bin)

let load_compressed_encrypted_file path tgtuser =
  let b0 = Cstdio.File.Buffer.create (64*1024) in
  let (bsz, bcrypt) = load_file_parts path b0 0 in
  (* Printf.printf "loaded from %s: %d\n" (Filesystem.Path.to_string path) bsz; *)
  if bsz == 0 then None
  else
  unencrypt bcrypt tgtuser |> function
  | None -> None
  | Some bcomp ->
    let b = uncompress bcomp in
    Some b

(* TODO *)
let compress bin =
  Printf.printf "compress sz in = %d\n" (Cstdio.File.Buffer.size bin);
  bin

(* TODO *)
let encrypt bin tgtuser =
  Printf.printf "encrypt for %s\n" tgtuser;
  Some bin

let save_compressed_encrypted_file bin path tgtuser =
  let bcomp = compress bin in
  encrypt bcomp tgtuser |> function
  | None -> Printf.eprintf "encryption failed\n"; false
  | Some benc ->
    let path' = Filesystem.Path.to_string path in
    Cstdio.File.fopen path' "wx" |> function
    | Error (ec,es) -> Printf.eprintf "error %d: %s\n" ec es; false
    | Ok file ->
      let res = Cstdio.File.fwrite benc (Cstdio.File.Buffer.size benc) file |> function
        | Ok _n -> true
        | Error _ -> false
      in
      Cstdio.File.fflush file |> ignore;
      Cstdio.File.fclose file |> ignore; res
