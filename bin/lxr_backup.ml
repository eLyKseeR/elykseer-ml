
(* open Elykseer *)
open Elykseer__Lxr
open Elykseer__Lxr.Backup

let conf0 : configuration = {
        num_chunks = Conversion.i2p 16;
        path_chunks = "./lxr";
        path_meta = "./meta" }

let e2s e = "Environment: cur_assembly = " ^ string_of_int (Conversion.p2i (Assembly.aid (cur_assembly e)))
          ^ ", count_input_bytes = " ^ string_of_int (Conversion.n2i (count_input_bytes e))
          ^ ", |files| = " ^ string_of_int (List.length (files e))
          ^ ", |blocks| = " ^ string_of_int (List.length (blocks e))
          ^ "\n."

let () =
        let e0 = initial_environment conf0 in
        let e1 = backup_file conf0 e0 "t1.dat" in
        print_string (e2s e1)
