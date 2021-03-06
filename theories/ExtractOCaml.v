
Require Coq.extraction.Extraction.
Extraction Language OCaml.

(* From Coq Require Import Arith.Arith.
From Coq Require Import Init.Nat.
From Coq Require Import Arith.EqNat. *)
Require Import ZArith NArith.
Open Scope positive_scope.

From LXR Require Import Assembly.
From LXR Require Import BackupPlanner.
From LXR Require Import Block.
From LXR Require Import Buffer.
From LXR Require Import Configuration.
From LXR Require Import Conversion.
From LXR Require Import Environment.
From LXR Require Import Filesupport.
From LXR Require Import Filetypes.

From Coq Require Import ExtrOcamlBasic.
From Coq Require Import ExtrOcamlNativeString.


Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inductive sumbool => "bool" ["true" "false"].
Extract Inductive option => option [ Some None ].

(** the following were found in: https://github.com/coq-contribs/zchinese 
    and are very useful to convert standard OCaml 'int' into positive|N|Z *)

Extract Inlined Constant int => "int". 

Extract Constant i2p =>
   "  
    let rec i2p = function 
       1 -> XH 
     | n -> let n' = i2p (n/2) in if (n mod 2)=0 then XO n' else XI n'
     in i2p
   ".
 
Extract Constant p2i =>
   "
    let rec p2i = function 
       XH -> 1
     | XO p -> 2*(p2i p)
     | XI p -> 2*(p2i p)+1
     in p2i 
   ".

Extract Constant i2z =>
   "
    function 
      0 -> Z0
    | n -> if n < 0 then Zneg (i2p (-n)) else Zpos (i2p n)
   ".

Extract Constant z2i =>
   "
    function
      Z0 -> 0 
    | Zpos p -> p2i p
    | Zneg p -> -(p2i p)
   ".

Extract Constant i2n =>
   "
    function 
      0 -> N0
    | n -> Npos (i2p n)
   ".

Extract Constant n2i =>
   "
    function
      N0 -> 0 
    | Npos p -> p2i p
   ".

Set Extraction AccessOpaque.

(* accessing information about a realistic file *)
Extract Constant get_file_information =>
   "  
    fun fn ->
        { Filetypes.fname = fn;
          Filetypes.fsize = Conversion.i2n (Elykseer_base.Fsutils.fsize fn);
          Filetypes.fowner = string_of_int (Elykseer_base.Fsutils.fowner fn);
          Filetypes.fpermissions = Conversion.i2n (Elykseer_base.Fsutils.fperm fn);
          Filetypes.fmodified = Elykseer_base.Fsutils.fmod fn;
          Filetypes.fchecksum = Elykseer_base.Fsutils.fchksum fn }
   ".

(* extract into "lxr.ml" all named modules and definitions, and their dependencies *)
Extraction "lxr.ml" BackupPlanner Block Configuration Conversion Environment backup_file.
