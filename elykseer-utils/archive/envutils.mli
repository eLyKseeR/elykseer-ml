
(* open Elykseer__Lxr *)
open Elykseer__Lxr.Configuration
open Elykseer__Lxr.Environment

val envload : configuration -> string -> environment

val envrestore : string -> environment

val env2assemblies : environment -> environment list