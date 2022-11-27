


(* helper routines for extraction from assoc lists *)
let get_int nm bs =
  match List.assoc nm bs with
  | `String s -> int_of_string s
  | _ -> 42
let get_str nm bs =
  match List.assoc nm bs with
  | `String s -> s
  | _ -> "unk"
let get_obj nm bs =
  match List.assoc nm bs with
  | `O os -> os
  | _ -> []
let get_arr nm bs =
  match List.assoc nm bs with
  | `A ls -> ls
  | _ -> []

