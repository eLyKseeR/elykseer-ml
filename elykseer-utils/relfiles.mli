open Elykseer__Lxr

type t

type relation = {
    rfi : Filetypes.fileinformation;
    rfbs : Assembly.blockinformation list
}

val new_map : Configuration.configuration -> t Lwt.t
val add : string -> relation -> t -> t Lwt.t
val find : string -> t -> relation option Lwt.t
val close_map : t -> unit Lwt.t
