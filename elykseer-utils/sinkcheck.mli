(* checks for the sink configuration used by lxr_distribute *)

(* true for localhost and loopback addresses *)
val is_local_host : string -> bool

(* only "https" is accepted, unless the host is local or [insecure] is set;
   then also "http" *)
val check_protocol : insecure:bool -> host:string -> string -> (unit, string) result

(* looks up a credential [key] in a JSON object: "<key>-env" names an
   environment variable that holds the secret and takes precedence over
   the inline value "<key>" *)
val resolve_secret : ?getenv:(string -> string option) -> Yojson.Basic.t -> string -> string option

(* true if the JSON object holds an inline (not from env) value for [key] *)
val has_inline_secret : Yojson.Basic.t -> string -> bool

(* a configuration file with inline secrets must not be accessible by
   group or others (e.g. chmod 600), unless [insecure] is set *)
val check_config_mode : insecure:bool -> inline_secrets:bool -> string -> (unit, string) result
