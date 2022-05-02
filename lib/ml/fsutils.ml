  open Unix

  let fsize fn = (Unix.stat fn).st_size
  let fperm fn = (Unix.stat fn).st_perm
  let fowner fn = (Unix.stat fn).st_uid

  let strftime tm = Printf.sprintf "%4d-%02d-%02d %02d:%02d:%02d"
                                    (tm.tm_year+1900) tm.tm_mon tm.tm_mday
                                    tm.tm_hour tm.tm_min tm.tm_sec
  let fmod fn = (stat fn).st_mtime |> gmtime |> strftime
