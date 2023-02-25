open Elykseer__Lxr

type t

type aid = string

val new_map : Configuration.configuration -> t Lwt.t
val add : aid -> Assembly.keyinformation -> t -> t Lwt.t
val find : aid -> t -> Assembly.keyinformation option Lwt.t
val find_v : aid -> t -> (aid * Assembly.keyinformation) option Lwt.t
val close_map : t -> unit Lwt.t
