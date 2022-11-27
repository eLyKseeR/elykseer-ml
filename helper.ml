
external cpp_buffer_id : 'a -> 'b = "cpp_buffer_id"

external cpp_encrypt_buffer : 'b1 -> string -> 'b2 = "cpp_encrypt_buffer"
external cpp_decrypt_buffer : 'b1 -> string -> 'b2 = "cpp_decrypt_buffer"

(** the chunk will be stored in a subdirectory
    which is the last two chars of the cid
*)
external mk_cid_subdir : string -> string = "cpp_mk_cid_subdir"

let store_chunk_to_path fp sz pos b =
  if Mlcpp_filesystem.Filesystem.Path.exists fp
  then 0
  else let dir = Mlcpp_filesystem.Filesystem.Path.parent fp in
       Mlcpp_filesystem.Filesystem.create_directories dir |> ignore;
       match Mlcpp_cstdio.Cstdio.File.fopen (Mlcpp_filesystem.Filesystem.Path.to_string fp) "wx" with
       | Error _ -> 0
       | Ok fptr -> let buf = Mlcpp_cstdio.Cstdio.File.Buffer.create sz in
                    let _n = Mlcpp_cstdio.Cstdio.File.Buffer.copy_sz_pos b ~pos1:pos ~sz:sz buf ~pos2:0 in
                    let res = match Mlcpp_cstdio.Cstdio.File.fwrite buf sz fptr with
                              | Error _ -> 0
                              | Ok cnt -> cnt
                    in
                    Mlcpp_cstdio.Cstdio.File.fflush fptr |> ignore;
                    Mlcpp_cstdio.Cstdio.File.fclose fptr |> ignore;
                    Mlcpp_cstdio.Cstdio.File.Buffer.release buf |> ignore;
                    res

let load_chunk_from_path fp =
  let b = Mlcpp_cstdio.Cstdio.File.Buffer.create (256*1024) in
  if Mlcpp_filesystem.Filesystem.Path.exists fp
  then Mlcpp_cstdio.Cstdio.File.fopen (Mlcpp_filesystem.Filesystem.Path.to_string fp) "rx" |> function
    | Error _ -> None
    | Ok fptr -> begin
        Mlcpp_cstdio.Cstdio.File.fread b (256*1024) fptr |> function
        | Error _ -> Mlcpp_cstdio.Cstdio.File.fclose fptr |> ignore; None
        | Ok _cnt -> Mlcpp_cstdio.Cstdio.File.fclose fptr |> ignore; Some b
    end
  else None