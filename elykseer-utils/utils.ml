
open Elykseer__Lxr
open Elykseer__Lxr.Assembly
open Elykseer__Lxr.Configuration
(* open Elykseer__Lxr.Conversion *)

let as2s (a:assemblyinformation) = "Assembly: nchunks = " ^ (string_of_int @@ Conversion.p2i @@ a.nchunks)
           ^ ", aid = " ^ a.aid
           ^ ", apos = " ^ (string_of_int @@ Conversion.n2i @@ a.apos)

let as2j a = "{ \"aid\": \"" ^ a.aid ^ "\""
           ^ ", \"nchunks\": " ^ (string_of_int @@ Conversion.p2i @@ a.nchunks)
           ^ ", \"apos\": " ^ (string_of_int @@ Conversion.n2i @@ a.apos) ^ " ] }"

let c2s c = "Configuration: config_nchunks = " ^ (string_of_int @@ Conversion.n2i @@ Nchunks.to_N c.config_nchunks)
          ^ ", path_chunks = " ^ c.path_chunks
          ^ ", path_meta = " ^ c.path_meta
          ^ ", my_id = " ^ (string_of_int @@ Conversion.n2i @@ c.my_id)

let c2j c = "{ \"config_nchunks\": \"" ^ (string_of_int @@ Conversion.n2i @@ Nchunks.to_N c.config_nchunks) ^ "\""
          ^ ", \"path_chunks\": \"" ^ c.path_chunks ^ "\""
          ^ ", \"path_meta\": \"" ^ c.path_meta ^ "\""
          ^ ", \"my_id\": " ^ (string_of_int @@ Conversion.n2i @@ c.my_id) ^ " }"
