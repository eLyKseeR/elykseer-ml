
open Mlcpp_cstdio
open Elykseer__Lxr

type t

val create : Configuration.configuration -> t Lwt.t
val addblock : t -> string -> Assembly.blockinformation -> Cstdio.File.Buffer.ta -> (t * Assembly.blockinformation) Lwt.t
val stop : t -> unit Lwt.t
