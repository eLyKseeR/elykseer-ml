
open Elykseer__Lxr
open Elykseer__Lxr.Assembly
open Elykseer__Lxr.Block
open Elykseer__Lxr.Configuration
open Elykseer__Lxr.Environment
open Elykseer__Lxr.Filetypes

let fi2s fi = "Fileinformation: fname = \"" ^ fi.fname ^ "\""
            ^ ", fsize = " ^ (string_of_int @@ Conversion.n2i @@ fi.fsize)
            ^ ", fowner = " ^ fi.fowner
            ^ ", fpermissions = " ^ (Printf.sprintf "%#o" @@ Conversion.n2i @@ fi.fpermissions)
            ^ ", fmodified = \"" ^ fi.fmodified ^ "\""
            ^ ", fchecksum = \"" ^ fi.fchecksum ^ "\""

let fi2j fi = "{ \"fname\":  \"" ^ fi.fname ^ "\""
            ^ ", \"fsize\": " ^ (string_of_int @@ Conversion.n2i @@ fi.fsize)
            ^ ", \"fowner\": \"" ^ fi.fowner ^ "\""
            ^ ", \"fpermissions\": " ^ (Printf.sprintf "\"0o%#o\"" @@ Conversion.n2i @@ fi.fpermissions)
            ^ ", \"fmodified\": \"" ^ fi.fmodified ^ "\""
            ^ ", \"fchecksum\": \"" ^ fi.fchecksum ^ "\" }"

let b2s b = "Block: blockid = " ^ (string_of_int @@ Conversion.p2i @@ b.blockid)
          ^ ", bchecksum = " ^ b.bchecksum
          ^ ", blocksize = " ^ (string_of_int @@ Conversion.n2i @@ b.blocksize)
          ^ ", filepos = " ^ (string_of_int @@ Conversion.n2i @@ b.filepos)
          ^ ", blockanum = " ^ (string_of_int @@ Conversion.p2i @@ b.blockanum)
          ^ ", blockapos = " ^ (string_of_int @@ Conversion.n2i @@ b.blockapos)

let b2j b = "{ \"blockid\": " ^ (string_of_int @@ Conversion.p2i @@ b.blockid)
          ^ ", \"bchecksum\": \"" ^ b.bchecksum ^ "\""
          ^ ", \"blocksize\": " ^ (string_of_int @@ Conversion.n2i @@ b.blocksize)
          ^ ", \"filepos\": " ^ (string_of_int @@ Conversion.n2i @@ b.filepos)
          ^ ", \"blockanum\": " ^ (string_of_int @@ Conversion.p2i @@ b.blockanum)
          ^ ", \"blockapos\": " ^ (string_of_int @@ Conversion.n2i @@ b.blockapos) ^ " }"

let fibs2s fibs = "\n  " ^ fi2s (bfi fibs)
                ^ "\n  version = " ^ (string_of_int @@ Conversion.p2i @@ fibs.fversion)
                ^ ", blocks = " ^ (string_of_int @@ List.length @@ blocks fibs) ^ "@[" ^ (String.concat "; " (List.map b2s (List.rev (blocks fibs)))) ^ "]"

let fibs2j fibs = "{ \"fi\": " ^ fi2j fibs.bfi
                ^ ", \"version\": " ^ (string_of_int @@ Conversion.p2i @@ fibs.fversion)
                ^ ", \"blocks\": [ " ^ (String.concat ", " (List.map b2j (List.rev fibs.blocks))) ^ " ] }"

let ch2s c = "Chunk:  cid = " ^ (string_of_int @@ Conversion.p2i @@ c.cid)
           ^ ", in_anum = " ^ (string_of_int @@ Conversion.p2i @@ c.in_anum)
           ^ ", buffer = " ^ (string_of_int @@ Conversion.n2i @@ Buffer.len c.buffer)
let ch2j c = "{ \"cid\": " ^ (string_of_int @@ Conversion.p2i @@ c.cid)
           ^ ", \"in_anum\": " ^ (string_of_int @@ Conversion.p2i @@ c.in_anum) ^ " }"

let as2s a = "Assembly: nchunks = " ^ (string_of_int @@ Conversion.p2i @@ a.nchunks)
           ^ ", anum = " ^ (string_of_int @@ Conversion.p2i @@ a.anum)
           ^ ", aid = " ^ a.aid
           ^ ", apos = " ^ (string_of_int @@ Conversion.n2i @@ a.apos)
           ^ ", encrypted = " ^ (if a.encrypted then "1" else "0")
           ^ ", chunks = " ^ (string_of_int @@ List.length @@ chunks a) ^ "@[" ^ (String.concat "; " (List.map ch2s (List.rev (chunks a)))) ^ "]"

let as2j a = "{ \"anum\": " ^ (string_of_int @@ Conversion.p2i @@ a.anum)
           ^ ", \"aid\": \"" ^ a.aid ^ "\""
           ^ ", \"nchunks\": " ^ (string_of_int @@ Conversion.p2i @@ a.nchunks)
           ^ ", \"apos\": " ^ (string_of_int @@ Conversion.n2i @@ a.apos)
           ^ ", \"encrypted\": " ^ (if a.encrypted then "1" else "0")
           ^ ", \"chunks\": [ " ^ (String.concat ", " (List.map ch2j @@ (chunks a))) ^ " ] }"

let c2s c = "Configuration: num_chunks = " ^ (string_of_int @@ Conversion.p2i @@ c.num_chunks)
          ^ ", path_chunks = " ^ c.path_chunks
          ^ ", path_meta = " ^ c.path_meta
          ^ ", my_id = " ^ (string_of_int @@ Conversion.n2i @@ c.my_id)

let c2j c = "{ \"num_chunks\": " ^ (string_of_int @@ Conversion.p2i @@ c.num_chunks)
          ^ ", \"path_chunks\": \"" ^ c.path_chunks ^ "\""
          ^ ", \"path_meta\": \"" ^ c.path_meta ^ "\""
          ^ ", \"my_id\": " ^ (string_of_int @@ Conversion.n2i @@ c.my_id) ^ " }"

let e2s e = "Environment: cur_assembly = " ^ (string_of_int @@ Conversion.p2i @@ Assembly.anum @@ cur_assembly e)
          ^ ", count_input_bytes = " ^ (string_of_int @@ Conversion.n2i @@ count_input_bytes e)
          ^ ", configuration = " ^ (c2s @@ config e)
          ^ ", files = " ^ (string_of_int @@ List.length @@ files e) ^ "@[" ^ (String.concat "; " (List.map fibs2s (List.rev (files e)))) ^ "]"
          ^ ", assemblies = " ^ (string_of_int @@ List.length @@ files e) ^ "@[" ^ (String.concat "; " (List.map as2s (List.rev (assemblies e)))) ^ "]"
          ^ "\n."

let e2j e = "{ \"cur_assembly\": { \"anum\": " ^ (string_of_int @@ Conversion.p2i @@ Assembly.anum @@ cur_assembly e)
                              ^ ", \"aid\": \"" ^ (Assembly.aid @@ cur_assembly e) ^ "\" }"
          ^ ", \"count_input_bytes\": " ^ (string_of_int @@ Conversion.n2i @@ count_input_bytes e)
          ^ ", \"configuration\": " ^ (c2j @@ config e)
          ^ ", \"files\": [ " ^ (String.concat ", " (List.map fibs2j (List.rev (files e)))) ^ " ]"
          ^ ", \"assemblies\": [ " ^ (String.concat ", " (List.map as2j @@ List.filter (fun a -> a.apos > Conversion.i2n 0) (List.rev (assemblies e)))) ^ " ] }"
