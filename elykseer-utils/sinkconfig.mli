open Elykseer__Lxr

(* parses the sink configuration of lxr_distribute (see sinks.json);
   an invalid sink is reported on stderr and returned as None *)
val from_json_sinks : insecure:bool -> Configuration.configuration -> Yojson.Basic.t -> Distribution.sink_type option list

(* true if any sink holds its access or secret key inline, not from env *)
val has_inline_secrets : Yojson.Basic.t -> bool

(* for the retry policy of a sink *)
val nat_of_int : int -> nat
val int_of_nat : nat -> int
