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

Record afblocks : Type :=
    mkafblocks
        { anum : positive
        ; afree : N
        ; ablocks : list fileblock
        }.

Section prepare_blocks.

Variable nchunks : positive.
Variable maxsz : N.
Definition aminsz : N := 64.

Fixpoint prepare_blocks (fuel : nat) (afb : afblocks) (fpos : N) (fsz : N) : afblocks :=
  (* Printf.printf "fsz: %d  afree: %d  maxsz: %d\n" fsz afree maxsz; *)
  match fuel with
  | O => afb
  | S f' =>
    if (fsz <=? afree afb)%N
    then
        if (maxsz <=? fsz)%N (* fsz >= maxsz *)
        then let newblock := {| fbanum := anum afb; fbfpos := fpos; fbsz := maxsz |} in
             prepare_blocks f' {| anum := anum afb; afree := afree afb - maxsz; ablocks := newblock :: ablocks afb |}
                            (fpos + maxsz) (fsz - maxsz)
        else (* termination *)
            if (0 <? fsz)%N (* fsz > 0 *) then {| anum := anum afb; afree := afree afb - fsz; ablocks := {| fbanum := anum afb; fbfpos := fpos; fbsz := fsz |} :: ablocks afb |}
            else afb
    else (* when fsz > afree *)
        if (maxsz <=? afree afb)%N (* afree >= maxsz *)
        then let newblock := {| fbanum := anum afb; fbfpos := fpos; fbsz := maxsz |} in
             prepare_blocks f' {| anum := anum afb; afree := afree afb - maxsz; ablocks := newblock :: ablocks afb |}
                            (fpos + maxsz) (fsz - maxsz)
        else (* afree < maxsz *)
        let asz := Assembly.assemblysize nchunks in
        if (afree afb <? aminsz)%N
            (* continue in fresh assembly *)
        then prepare_blocks f' {| anum := anum afb + 1; afree := asz; ablocks := ablocks afb |} fpos fsz
        else let newblock := {| fbanum := anum afb; fbfpos := fpos; fbsz := afree afb |} in
             prepare_blocks f' {| anum := anum afb + 1; afree := asz; ablocks := newblock :: ablocks afb |}
                            (fpos + (afree afb)) (fsz - (afree afb))
  end.

End prepare_blocks.

Section analyse_file.

Variable nchunks : positive.
Definition max_block_size : N := 131072.

Definition analyse_file (afree_p : N) (anum_p : positive) (fn : string) : afblocks :=
  (* Printf.printf "backup %s\n" fn; *)
  let fi := Filesupport.get_file_information fn in
  let nblocks := (fsize fi) / max_block_size in
  let fuel : nat := N.to_nat (nblocks + (nblocks / Conversion.pos2N nchunks) + 1) in
  let afbs := prepare_blocks nchunks max_block_size fuel {|anum:=anum_p;afree:=afree_p;ablocks:=nil|} 0 (fsize fi) in
  {| anum := anum afbs; afree := afree afbs; ablocks := List.rev (ablocks afbs) |}.

End analyse_file.

End BackupPlanner.
