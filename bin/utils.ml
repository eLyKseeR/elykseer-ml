
open Elykseer__Lxr
open Elykseer__Lxr.Backup
open Elykseer__Lxr.Filetypes

let fi2s fi = "Fileinformation: fname = \"" ^ fi.fname ^ "\""
            ^ ", fsize = " ^ (string_of_int @@ Conversion.n2i @@ fi.fsize)
            ^ ", fowner = " ^ fi.fowner
            ^ ", fpermissions = " ^ (Printf.sprintf "%#o" @@ Conversion.n2i @@ fi.fpermissions)
            ^ ", fmodified = \"" ^ fi.fmodified ^ "\""
            ^ ", fchecksum = \"" ^ fi.fchecksum ^ "\""

let b2s b = "Block: blockid = " ^ (string_of_int @@ Conversion.n2i @@ b.blockid)
          ^ ", blocksize = " ^ (string_of_int @@ Conversion.n2i @@ b.blocksize)
          ^ ", blockaid = " ^ (string_of_int @@ Conversion.p2i @@ b.blockaid)

let e2s e = "Environment: cur_assembly = " ^ (string_of_int @@ Conversion.p2i @@ Assembly.aid @@ cur_assembly e)
          ^ ", count_input_bytes = " ^ (string_of_int @@ Conversion.n2i @@ count_input_bytes e)
          ^ ", files = " ^ (string_of_int @@ List.length @@ files e) ^ "@[" ^ (String.concat "; " (map fi2s (files e))) ^ "]"
          ^ ", blocks = " ^ (string_of_int @@ List.length @@ blocks e) ^ "@[" ^ (String.concat "; " (map b2s (blocks e))) ^ "]"
          ^ "\n."

