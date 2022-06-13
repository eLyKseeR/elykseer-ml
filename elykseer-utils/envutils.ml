
open Elykseer__Lxr
open Elykseer__Lxr.Block
(* open Elykseer__Lxr.Configuration *)
open Elykseer__Lxr.Environment
open Elykseer__Lxr.Filetypes

(* open Yojson.Basic *)
open Yojson.Basic.Util

module JsonTr = struct
    let tr_fi json = 
        { fname = json |> member "fname" |> to_string;
          fsize = json |> member "fsize" |> to_int |> Conversion.i2n;
          fowner = json |> member "fowner" |> to_string;
          fpermissions = json |> member "fpermissions" |> to_int |> Conversion.i2n;
          fmodified = json |> member "fmodified" |> to_string;
          fchecksum = json |> member "fchecksum" |> to_string;
        }
    let tr_block json =
        { blockid = json |> member "blockid" |> to_int |> Conversion.i2p;
          blocksize = json |> member "blocksize" |> to_int |> Conversion.i2n;
          filepos = json |> member "filepos" |> to_int |> Conversion.i2n;
          blockaid = json |> member "blockaid" |> to_int |> Conversion.i2p;
          blockapos = json |> member "blockapos" |> to_int |> Conversion.i2n;
        }
    let rec tr_blocks acc bs =
        match bs with
        | [] -> List.rev acc
        | b :: bs' -> tr_blocks (tr_block b :: acc) bs'
    let tr_fileblocks json =
        { bfi = json |> member "bfi" |> tr_fi;
          blocks = json |> member "blocks" |> to_list |> tr_blocks [];
        }
    let rec tr_files acc fs =
        match fs with
        | [] -> List.rev acc
        | f :: fs' -> tr_files (tr_fileblocks f :: acc) fs'
end

let envload c fn =
    let env0 = initial_environment c in
    let json = Yojson.Basic.from_file fn in
        { env0 with count_input_bytes = json |> member "count_input_bytes" |> to_int |> Conversion.i2n;
                    files = json |> member "files" |> to_list |> JsonTr.tr_files []; }

