open Elykseer__Lxr

type t

val new_map : Configuration.configuration -> t Lwt.t
val add : string -> Assembly.keyinformation -> t -> t Lwt.t
val find : string -> t -> Assembly.keyinformation option Lwt.t
val close_map : t -> unit Lwt.t
