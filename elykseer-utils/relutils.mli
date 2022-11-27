
val get_int : 'a -> ('a * [> `String of string ]) list -> int
val get_str : 'a -> ('a * [> `String of string ]) list -> string
val get_obj : 'a -> ('a * [> `O of 'b list]) list -> 'b list
val get_arr : 'a -> ('a * [> `A of 'b list]) list -> 'b list
