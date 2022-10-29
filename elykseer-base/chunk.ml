open Mlcpp_cstdio
open Mlcpp_filesystem

type t = Cstdio.File.Buffer.ta

let size = Cstdio.File.Buffer.size

let get = Cstdio.File.Buffer.get

let set = Cstdio.File.Buffer.set

let load_from_path sfp =
  Printf.printf "load_from_path %s\n" sfp;
  let fp = Filesystem.Path.from_string sfp in
  if Filesystem.Path.exists fp && Filesystem.Path.is_regular_file fp
  then
    let fsz = Filesystem.Path.file_size fp in
    let buf = Cstdio.File.Buffer.create fsz in
    Cstdio.File.fopen (Filesystem.Path.to_string fp) "rx" |> function
      | Ok fptr -> begin
          Cstdio.File.fread buf fsz fptr |> function
            | Ok _nread -> Some buf
            | Error (errno,errstr) ->
              Printf.printf "error:%d %s\n" errno errstr; None
        end
      | Error (errno,errstr) ->
        Printf.printf "error:%d %s\n" errno errstr; None
  else begin
    Printf.printf "not existing: %s\n" sfp; None
  end
