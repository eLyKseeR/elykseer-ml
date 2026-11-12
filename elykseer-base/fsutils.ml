  open Unix

  let fsize fn = (Unix.stat fn).st_size
  (* the permission bits written as octal digits, e.g. 0o640 -> 640 *)
  let fperm fn = int_of_string (Printf.sprintf "%o" (Unix.stat fn).st_perm)
  let fowner fn = (Unix.stat fn).st_uid

  let strftime tm = Printf.sprintf "%4d-%02d-%02d %02d:%02d:%02d"
                                    (tm.tm_year+1900) (tm.tm_mon+1) tm.tm_mday
                                    tm.tm_hour tm.tm_min tm.tm_sec
  let fmod fn = (stat fn).st_mtime |> gmtime |> strftime

  let fchksum fn = Elykseer_crypto.Sha3_256.file fn
