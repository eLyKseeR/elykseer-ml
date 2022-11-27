(**
      e L y K s e e R
*)

Module Export Environment.

Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String.

Open Scope positive_scope.
Open Scope N_scope.

From LXR Require Import Assembly.
From LXR Require Import Buffer.
From LXR Require Import Configuration.
From LXR Require Import Filetypes.


Record environment : Type :=
    mkenvironment
        { cur_assembly : AssemblyPlainWritable.H
        ; cur_buffer : AssemblyPlainWritable.B
        ; config : configuration
        ; fblocks : list (string * Assembly.blockinformation)
        ; keys : list (string * Assembly.keyinformation)
        }.

Definition initial_environment (c : configuration) : environment :=
    let (a,b) := AssemblyPlainWritable.create c in
    {| cur_assembly := a
    ;  cur_buffer := b
    ;  config := c
    ;  fblocks := nil
    ;  keys := nil
    |}.

Definition recreate_assembly (e : environment) : environment :=
    let (a,b) := AssemblyPlainWritable.create (config e) in
    {| cur_assembly := a
    ;  cur_buffer := b
    ;  config := config e
    ;  fblocks := fblocks e
    ;  keys := keys e
    |}.
Definition env_add_file_block (fname : string) (e : environment) (bi : Assembly.blockinformation) : environment :=
    {| cur_assembly := cur_assembly e
    ;  cur_buffer := cur_buffer e
    ;  config := config e
    ;  fblocks := (fname,bi) :: fblocks e
    ;  keys := keys e
    |}.

Definition env_add_aid_key (aid : string) (e : environment) (ki : keyinformation) : environment :=
    {| cur_assembly := cur_assembly e
    ;  cur_buffer := cur_buffer e
    ;  config := config e
    ;  fblocks := fblocks e
    ;  keys := (aid,ki) :: keys e
    |}.

Program Definition backup (e0 : environment) (fp : string) (fpos : N) (content : BufferPlain.buffer_t) : environment :=
    let (a', bi) := Assembly.backup (cur_assembly e0) (cur_buffer e0) fpos content in
    {| cur_assembly := a'
    ;  cur_buffer := cur_buffer e0
    ;  config := config e0
    ;  fblocks := (fp,bi) :: fblocks e0
    ;  keys := keys e0
    |}.

End Environment.