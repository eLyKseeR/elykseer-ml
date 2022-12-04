open Elykseer__Lxr

type t

type relation = {
    rfi : Filetypes.fileinformation;
    rfbs : Assembly.blockinformation list
}

type filehash = string

val new_map : Configuration.configuration -> t Lwt.t
val add : filehash -> relation -> t -> t Lwt.t
val find : filehash -> t -> relation option Lwt.t
val close_map : t -> unit Lwt.t
