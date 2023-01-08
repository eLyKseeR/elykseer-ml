open Elykseer_crypto
open Mlcpp_cstdio
open Mlcpp_filesystem

external cpp_buffer_id : 'a -> 'b = "cpp_buffer_id"

external cpp_encrypt_aes256 : Key128.t -> Key256.t -> 'b1 -> 'b2 = "cpp_encrypt_aes256"
let cpp_encrypt_buffer b siv spk =
  let iv = Key128.from_hex siv in
  let pk = Key256.from_hex spk in
  cpp_encrypt_aes256 iv pk b

external cpp_decrypt_aes256 : Key128.t -> Key256.t -> 'b1 -> 'b2 = "cpp_decrypt_aes256"
let cpp_decrypt_buffer b siv spk =
  let iv = Key128.from_hex siv in
  let pk = Key256.from_hex spk in
  cpp_decrypt_aes256 iv pk b
  
(** the chunk will be stored in a subdirectory
    which is the last two chars of the cid (cid[-2], cid[-1]),
    in a subdirectory (cid[-4], cid[-3])
*)
external mk_cid_subdir : string -> string = "cpp_mk_cid_subdir"

let store_chunk_to_path fp sz pos b =
  if Filesystem.Path.exists fp
  then let () = Printf.printf "chunk exists: %s\n" (Filesystem.Path.to_string fp) in 0
  else let dir = Filesystem.Path.parent fp in
       Filesystem.create_directories dir |> ignore;
       match Cstdio.File.fopen (Filesystem.Path.to_string fp) "wx" with
       | Error (errno,errstr) -> let () = Printf.printf "chunk fopen error no:%d err:%s\n" errno errstr in 0
       | Ok fptr -> let buf = Cstdio.File.Buffer.create sz in
                    let _n = Cstdio.File.Buffer.copy_sz_pos b ~pos1:pos ~sz:sz buf ~pos2:0 in
                    let res = match Cstdio.File.fwrite buf sz fptr with
                              | Error (errno,errstr) -> let () = Printf.printf "chunk fwrite error no:%d err:%s\n" errno errstr in 0
                              | Ok cnt -> cnt
                    in
                    Cstdio.File.fflush fptr |> ignore;
                    Cstdio.File.fclose fptr |> ignore;
                    Cstdio.File.Buffer.release buf |> ignore;
                    res

let load_chunk_from_path fp =
  let b = Cstdio.File.Buffer.create (256*1024) in
  if Filesystem.Path.exists fp
  then Cstdio.File.fopen (Filesystem.Path.to_string fp) "rx" |> function
    | Error _ -> None
    | Ok fptr -> begin
        Cstdio.File.fread b (256*1024) fptr |> function
        | Error _ -> Cstdio.File.fclose fptr |> ignore; None
        | Ok _cnt -> Cstdio.File.fclose fptr |> ignore; Some b
    end
  else None

let cpp_mk_key256 () = Elykseer_crypto.Key256.mk () |> Elykseer_crypto.Key256.to_hex
let cpp_mk_key128 () = Elykseer_crypto.Key128.mk () |> Elykseer_crypto.Key128.to_hex
