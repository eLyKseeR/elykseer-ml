(**
      e L y K s e e R
*)

(** modeling backup of multiple files to LXR *)

Module Export BackupPlanner.

(** imports *)
From Coq Require Import Strings.String Strings.Byte Lists.List Lia.
(* Require Import Arith Number. *)
Require Import ZArith NArith PArith.
From Coq Require Import NArith.BinNat Program.Wf.
Open Scope positive_scope.
Open Scope N_scope.

From LXR Require Import Assembly.
From LXR Require Import Buffer.
From LXR Require Import Configuration.
From LXR Require Import Conversion.
From LXR Require Import Environment.
From LXR Require Import Filesupport.
From LXR Require Import Filetypes.

Open Scope string_scope.

Record fileblock : Type :=
    mkfileblock
        { fbanum : positive
        ; fbfpos : N
        ; fbsz : N
        }.

Record fileblockinformation : Type :=
    mkfbinfo
        { fbifi : fileinformation
        ; fbifblocks : list fileblock
        }.

Section prepare_blocks.

Variable nchunks : positive.
Variable maxsz : N.
Definition aminsz : N := 64.

Fixpoint prepare_blocks (fuel : nat) (anum_p : positive) (afree_p : N) (fbs_p : list fileblock) (fpos : N) (fsz : N) : list fileblock * (positive * N) :=
  (* Printf.printf "fsz: %d  afree: %d  maxsz: %d\n" fsz afree maxsz; *)
  match fuel with
  | O => (fbs_p, (anum_p, afree_p))
  | S f' =>
    if (fsz <=? afree_p)%N
    then
        if (maxsz <=? fsz)%N (* fsz >= maxsz *)
        then let newblock := {| fbanum := anum_p; fbfpos := fpos; fbsz := maxsz |} in
             prepare_blocks f' anum_p (afree_p - maxsz) (newblock :: fbs_p)
                            (fpos + maxsz) (fsz - maxsz)
        else (* termination *)
            if (0 <? fsz)%N (* fsz > 0 *) then ({| fbanum := anum_p; fbfpos := fpos; fbsz := fsz |} :: fbs_p
                                               , (anum_p, afree_p - fsz))
            else (fbs_p, (anum_p, afree_p))
    else (* when fsz > afree *)
        if (maxsz <=? afree_p)%N (* afree >= maxsz *)
        then let newblock := {| fbanum := anum_p; fbfpos := fpos; fbsz := maxsz |} in
             prepare_blocks f' anum_p (afree_p - maxsz) (newblock :: fbs_p)
                            (fpos + maxsz) (fsz - maxsz)
        else (* afree < maxsz *)
        let asz := Assembly.assemblysize nchunks in
        if (afree_p <? aminsz)%N
            (* continue in fresh assembly *)
        then prepare_blocks f' (anum_p + 1) asz fbs_p fpos fsz
        else let newblock := {| fbanum := anum_p; fbfpos := fpos; fbsz := afree_p |} in
             prepare_blocks f' (anum_p + 1) asz (newblock :: fbs_p)
                            (fpos + afree_p) (fsz - afree_p)
  end.

End prepare_blocks.

Section analyse_file.

Variable nchunks : positive.
Definition max_block_size : N := 131072.

Definition analyse_file (afree_p : N) (anum_p : positive) (fn : string) : (positive * N) * fileblockinformation :=
  (* Printf.printf "backup %s\n" fn; *)
  let fi := Filesupport.get_file_information fn in
  let nblocks := (fsize fi) / max_block_size in
  let fuel : nat := N.to_nat (nblocks + (nblocks / Conversion.pos2N nchunks) + 1) in
  let (afbs, ares) := prepare_blocks nchunks max_block_size fuel anum_p afree_p nil 0 (fsize fi) in
  (ares, {| fbifi := fi; fbifblocks := List.rev afbs |}).

End analyse_file.

End BackupPlanner.
