
open Assembly
open Backup
open FileSupport
open FileSupportImpl

let conf0 : configuration = {
        num_chunks = 16;
        path_chunks = "./lxr";
        path_meta = "./meta" }

let _ =
        let e0 = initial_environment conf0 in
        let e1 = backup_file conf0 e0 "t1.dat" in
        print_string e1
