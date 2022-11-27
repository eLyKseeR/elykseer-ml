open Elykseer__Lxr

type t

val new_map : Configuration.configuration -> t Lwt.t
val add : string -> Assembly.blockinformation list -> t -> t Lwt.t
val find : string -> t -> (Assembly.blockinformation list) option Lwt.t
val close_map : t -> unit Lwt.t
