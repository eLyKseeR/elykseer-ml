(* a missing field raises Not_found, a field of the wrong type raises Failure *)
val get_int : string -> (string * [> `String of string ]) list -> int
val get_str : string -> (string * [> `String of string ]) list -> string
val get_obj : string -> (string * [> `O of 'b list]) list -> 'b list
val get_arr : string -> (string * [> `A of 'b list]) list -> 'b list
