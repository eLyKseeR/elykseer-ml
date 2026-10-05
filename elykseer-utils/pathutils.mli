(* turns a file name given for restore into a relative path, like tar:
   leading '/' and '.' components are dropped, '..' components are rejected *)
val restore_relpath : string -> (string, string) result

(* true if path [p] equals or lies below directory [base];
   both are compared lexically and must be normalised already *)
val is_below : base:string -> string -> bool
