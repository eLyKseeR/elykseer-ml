
Require Coq.extraction.Extraction.
Extraction Language OCaml.

(* From Coq Require Import Arith.Arith.
From Coq Require Import Init.Nat.
From Coq Require Import Arith.EqNat. *)
Require Import ZArith NArith.
Open Scope positive_scope.

From LXR Require Import Assembly.
From LXR Require Import Backup.
From LXR Require Import Buffer.
From LXR Require Import Conversion.
From LXR Require Import FileSupport.
From LXR Require Import FileTypes.

From Coq Require Import ExtrOcamlBasic.
(* From Coq Require Import ExtrOCamlInt63. *)
From Coq Require Import ExtrOcamlNativeString.


Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inductive sumbool => "bool" ["true" "false"].
Extract Inductive option => option [ Some None ].

Extract Constant get_file_information => "FileSupportImpl.get_file_information".

Extraction "lxr_backup.ml" backup_file.
(* Separate Extraction backup_file. *)

Extraction "filesupport.ml" FileSupport.
Extraction "filetypes.ml" FileTypes.
