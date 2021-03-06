
open Elykseer__Lxr
open Elykseer__Lxr.Block
open Elykseer__Lxr.Environment
open Elykseer__Lxr.Filetypes

let fi2s fi = "Fileinformation: fname = \"" ^ fi.fname ^ "\""
            ^ ", fsize = " ^ (string_of_int @@ Conversion.n2i @@ fi.fsize)
            ^ ", fowner = " ^ fi.fowner
            ^ ", fpermissions = " ^ (Printf.sprintf "%#o" @@ Conversion.n2i @@ fi.fpermissions)
            ^ ", fmodified = \"" ^ fi.fmodified ^ "\""
            ^ ", fchecksum = \"" ^ fi.fchecksum ^ "\""

let b2s b = "Block: blockid = " ^ (string_of_int @@ Conversion.p2i @@ b.blockid)
          ^ ", blocksize = " ^ (string_of_int @@ Conversion.n2i @@ b.blocksize)
          ^ ", filepos = " ^ (string_of_int @@ Conversion.n2i @@ b.filepos)
          ^ ", blockaid = " ^ (string_of_int @@ Conversion.p2i @@ b.blockaid)
          ^ ", blockapos = " ^ (string_of_int @@ Conversion.n2i @@ b.blockapos)

let fibs2s fibs = "\n  " ^ fi2s (bfi fibs)
          ^ "\n                 , blocks = " ^ (string_of_int @@ List.length @@ blocks fibs) ^ "@[" ^ (String.concat "; " (List.map b2s (blocks fibs))) ^ "]"

let e2s e = "Environment: cur_assembly = " ^ (string_of_int @@ Conversion.p2i @@ Assembly.aid @@ cur_assembly e)
          ^ ", count_input_bytes = " ^ (string_of_int @@ Conversion.n2i @@ count_input_bytes e)
          ^ ", files = " ^ (string_of_int @@ List.length @@ files e) ^ "@[" ^ (String.concat "; " (List.map fibs2s (files e))) ^ "]"
          ^ "\n."

