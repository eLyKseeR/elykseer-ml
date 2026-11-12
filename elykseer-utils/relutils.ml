(* helper routines for extraction from assoc lists;
   a missing field raises Not_found, a field of the wrong type raises Failure *)
let wrong_type nm = failwith (nm ^ ": unexpected type")

let get_int nm bs =
  match List.assoc nm bs with
  | `String s -> int_of_string s
  | _ -> wrong_type nm
let get_str nm bs =
  match List.assoc nm bs with
  | `String s -> s
  | _ -> wrong_type nm
let get_obj nm bs =
  match List.assoc nm bs with
  | `O os -> os
  | _ -> wrong_type nm
let get_arr nm bs =
  match List.assoc nm bs with
  | `A ls -> ls
  | _ -> wrong_type nm
