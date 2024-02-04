  open Unix

  let fsize fn = (Unix.stat fn).st_size
  let fperm fn = let pdec = (Unix.stat fn).st_perm in
                 let rec as_octal = fun oct dec pow ->
                            if dec > 0 then
                              let rem = dec mod 8 in
                              let div = dec / 8 in
                              as_octal (oct + int_of_float(10. ** pow) * rem ) div (pow +. 1.)
                            else
                              oct
                 in as_octal 0 pdec 0.
  let fowner fn = (Unix.stat fn).st_uid

  let strftime tm = Printf.sprintf "%4d-%02d-%02d %02d:%02d:%02d"
                                    (tm.tm_year+1900) (tm.tm_mon+1) tm.tm_mday
                                    tm.tm_hour tm.tm_min tm.tm_sec
  let fmod fn = (stat fn).st_mtime |> gmtime |> strftime

  let fchksum fn = Elykseer_crypto.Sha256.file fn
