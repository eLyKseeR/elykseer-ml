
external cpp_buffer_id : 'a -> 'b = "cpp_buffer_id"


let store_chunk_to_path fp sz pos b =
  if Mlcpp_filesystem.Filesystem.Path.exists fp
  then 0
  else let buf = Mlcpp_cstdio.Cstdio.File.Buffer.create sz in
       let _n = Mlcpp_cstdio.Cstdio.File.Buffer.copy_sz_pos b pos sz buf 0 in
       match Mlcpp_cstdio.Cstdio.File.fopen (Mlcpp_filesystem.Filesystem.Path.to_string fp) "wx" with
       | Error _ -> 0
       | Ok fptr -> match Mlcpp_cstdio.Cstdio.File.fwrite buf sz fptr with
                    | Error _ -> 0
                    | Ok cnt -> cnt

let load_chunk_from_path fp =
  let b = Mlcpp_cstdio.Cstdio.File.Buffer.create (256*1024) in
  if Mlcpp_filesystem.Filesystem.Path.exists fp
  then Mlcpp_cstdio.Cstdio.File.fopen (Mlcpp_filesystem.Filesystem.Path.to_string fp) "rx" |> function
    | Error _ -> None
    | Ok fptr -> begin
        Mlcpp_cstdio.Cstdio.File.fread b (256*1024) fptr |> function
        | Error _ -> None
        | Ok _cnt -> Some b
    end
  else None