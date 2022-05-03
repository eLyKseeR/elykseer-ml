
(* open Elykseer *)
open Elykseer__Lxr
open Elykseer__Lxr.Backup

let conf0 : configuration = {
        num_chunks = Conversion.i2p 16;
        path_chunks = "./lxr";
        path_meta = "./meta" }

let () =
        let e0 = initial_environment conf0 in
        let e1 = backup_file conf0 e0 "elykseer.opam" in
        print_string (Utils.e2s e1)
