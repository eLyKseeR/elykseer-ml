open Elykseer__Lxr

(* wraps a tracer and counts the messages logged at level warning or error;
   every such message in the backup and restore paths reports a failure *)
val counting : Tracer.tracer -> Tracer.tracer * (unit -> int)
