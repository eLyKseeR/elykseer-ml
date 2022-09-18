
open Elykseer__Lxr
open Elykseer__Lxr.Assembly
open Elykseer__Lxr.Block
open Elykseer__Lxr.Configuration
open Elykseer__Lxr.Environment
open Elykseer__Lxr.Filetypes

(* open Yojson.Basic *)
open Yojson.Basic.Util

module JsonTr = struct
    let tr_cur_assembly json =
        { anum = json |> member "anum" |> to_int |> Conversion.i2p;
          aid = json |> member "aid" |> to_string;
          nchunks = Conversion.i2p 256;
          apos = Conversion.i2n 0;
          encrypted = false;
          chunks = [];
        }
    let tr_config json =
        { num_chunks = json |> member "num_chunks" |> to_int |> Conversion.i2p;
          path_chunks = json |> member "path_chunks" |> to_string;
          path_meta = json |> member "path_meta" |> to_string;
          my_id = json |> member "my_id" |> to_int |> Conversion.i2n;
        }
    let tr_fi json = 
        { fname = json |> member "fname" |> to_string;
          fsize = json |> member "fsize" |> to_int |> Conversion.i2n;
          fowner = json |> member "fowner" |> to_string;
          fpermissions = json |> member "fpermissions" |> to_string |> int_of_string |> Conversion.i2n;
          fmodified = json |> member "fmodified" |> to_string;
          fchecksum = json |> member "fchecksum" |> to_string;
        }
    let tr_block json =
        { blockid = json |> member "blockid" |> to_int |> Conversion.i2p;
          bchecksum = json |> member "bchecksum" |> to_string;
          blocksize = json |> member "blocksize" |> to_int |> Conversion.i2n;
          filepos = json |> member "filepos" |> to_int |> Conversion.i2n;
          blockanum = json |> member "blockanum" |> to_int |> Conversion.i2p;
          blockapos = json |> member "blockapos" |> to_int |> Conversion.i2n;
        }
    let rec tr_blocks acc bs =
        match bs with
        | [] -> (* List.rev *) acc
        | b :: bs' -> tr_blocks (tr_block b :: acc) bs'
    let tr_assembly json =
        { anum = json |> member "anum" |> to_int |> Conversion.i2p;
          aid = json |> member "aid" |> to_string;
          nchunks = json |> member "nchunks" |> to_int |> Conversion.i2p;
          apos = json |> member "apos" |> to_int |> Conversion.i2n;
          encrypted = json |> member "encrypted" |> to_int |> (fun x -> if x = 1 then true else false);
          chunks = [];
        }
    let rec tr_assemblies acc ass =
        match ass with
        | [] -> (* List.rev *) acc
        | a :: ass' -> tr_assemblies (tr_assembly a :: acc) ass'
    let tr_fileblocks json =
        { bfi = json |> member "fi" |> tr_fi;
          fversion = json |> member "version" |> to_int |> Conversion.i2p;
          blocks = json |> member "blocks" |> to_list |> tr_blocks [];
        }
    let rec tr_files acc fs =
        match fs with
        | [] -> (* List.rev *) acc
        | f :: fs' -> tr_files (tr_fileblocks f :: acc) fs'
end


let envload c fn =
    let env0 = initial_environment c in
    let json = Yojson.Basic.from_file fn in
        { env0 with count_input_bytes = json |> member "count_input_bytes" |> to_int |> Conversion.i2n;
                    files = json |> member "files" |> to_list |> JsonTr.tr_files []; }


let envrestore fn =
    let json = Yojson.Basic.from_file fn in
    { count_input_bytes = json |> member "count_input_bytes" |> to_int |> Conversion.i2n
    ; files = json |> member "files" |> to_list |> JsonTr.tr_files []
    ; config = json |> member "configuration" |> JsonTr.tr_config
    ; assemblies = json |> member "assemblies" |> to_list |> JsonTr.tr_assemblies []
    ; cur_assembly = json |> member "cur_assembly" |> JsonTr.tr_cur_assembly
    }


let env2assemblies e =
    let anums = List.rev @@ List.map (anum) e.assemblies in
    List.map (fun anum ->
        let assembly = e.assemblies |> List.filter (fun a -> a.anum = anum) |> List.hd in
        { cur_assembly = assembly
        ; count_input_bytes = Conversion.i2n 0
        ; config = config e
        ; files = e.files |> List.map (fun fb ->
                                (* list blocks in this assembly *)
                                let bs = fb.blocks |> List.filter (fun b ->
                                    b.blockanum = anum) in
                                { bfi = fb.bfi; fversion = Conversion.i2p 1; blocks = bs } ) |>
                                (* filter out files without blocks in this assembly *)
                                List.filter (fun fb -> fb.blocks <> [])
        ; assemblies = [assembly]
        }
    ) anums
