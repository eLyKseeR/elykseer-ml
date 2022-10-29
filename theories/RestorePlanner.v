(**
      e L y K s e e R
*)

(** modeling restore of assemblies *)

Module Export RestorePlanner.

(** imports *)
From Coq Require Import Strings.String Strings.Byte Lists.List Lia.
(* Require Import Arith Number. *)
Require Import ZArith NArith PArith.
From Coq Require Import NArith.BinNat.
Open Scope positive_scope.
Open Scope N_scope.

From LXR Require Import Assembly.
(* From LXR Require Import Block.
From LXR Require Import Configuration.
From LXR Require Import Conversion. *)
From LXR Require Import Environment.
(* From LXR Require Import Filesupport.
From LXR Require Import Filetypes. *)

Open Scope string_scope.


Definition restore_assembly (restore_aid : string) (e : environment) : option environment :=
      if (aid (cur_assembly e)) =? restore_aid then Some e
      else
        match List.find (fun a => aid a =? restore_aid) (assemblies e) with
        | None => None
        | Some a => Some (env_set_assembly e (Assembly.restore (config e) a))
        end.

End RestorePlanner.
