(* command line helpers shared by the lxr_* tools *)

(* the identifier used when "-i" is not given *)
val default_myid : string

(* the range of chunks per assembly, as enforced by Nchunks *)
val min_nchunks : int
val max_nchunks : int

(* Arg spec for "-n": rejects values outside [min_nchunks, max_nchunks]
   (Nchunks would silently clamp them) *)
val nchunks_spec : int ref -> Arg.spec

(* prints the message and the usage to stderr and exits with code 2 *)
val usage_error : (Arg.key * Arg.spec * Arg.doc) list -> string -> string -> 'a

(* fails with a usage error if one of the options (name, value) has
   an empty value, i.e. was not given on the command line *)
val require : (Arg.key * Arg.spec * Arg.doc) list -> string -> (string * string) list -> unit

(* true if [dbpath] holds a meta data database (irmin git store) *)
val db_exists : string -> bool

(* exits with code 2 if there is no database at [dbpath];
   otherwise irmin would silently create an empty one *)
val require_db : string -> unit

(* notes on stderr that a new database will be created at [dbpath] *)
val note_new_db : string -> unit

(* warns on stderr if the default identifier is used *)
val warn_default_myid : string -> unit
