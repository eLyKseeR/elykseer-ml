open Elykseer__Lxr

type t

type relation = {
    rfi : Filesupport.fileinformation;
    rfbs : Assembly.blockinformation list
}

type filehash = string

val new_map : Configuration.configuration -> t Lwt.t
(* fails if the store rejects the update *)
val add : filehash -> relation -> t -> (t, string) result Lwt.t
val find : filehash -> t -> relation option Lwt.t
val find_v : filehash -> t -> (string * relation) option Lwt.t
(* all file hashes of the current identifier, sorted *)
val hashes : t -> filehash list Lwt.t
val close_map : t -> unit Lwt.t
