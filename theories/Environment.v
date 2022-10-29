(**
      e L y K s e e R
*)

Module Export Environment.

Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String.

Open Scope positive_scope.
Open Scope N_scope.

From LXR Require Import Assembly.
From LXR Require Import Configuration.
From LXR Require Import Filetypes.
From LXR Require Import RelationAidKey RelationFileAid.


Record environment : Type :=
    mkenvironment
        { cur_assembly : AssemblyPlainWritable.H
        ; cur_buffer : AssemblyPlainWritable.B
        ; config : configuration
        ; files : RelationFileAid.Map
        ; keys : RelationAidKey.Map
        }.

Definition initial_environment (c : configuration) : environment :=
    let (a,b) := AssemblyPlainWritable.create c in
    {| cur_assembly := a
    ;  cur_buffer := b
    ;  config := c
    ;  files := RelationFileAid.new
    ;  keys := RelationAidKey.new
    |}.
Definition recreate_assembly (e : environment) : environment :=
    let (a,b) := AssemblyPlainWritable.create (config e) in
    {| cur_assembly := a
    ;  cur_buffer := b
    ;  config := config e
    ;  files := files e
    ;  keys := keys e
    |}.
Definition env_add_file_block (e : environment) (fname : string) (bi : blockinformation) : environment :=
    let entries :=
        match RelationFileAid.find fname (files e) with
        | Some bis => bi :: bis
        | None => bi :: nil
        end in
    {| cur_assembly := cur_assembly e
    ;  cur_buffer := cur_buffer e
    ;  config := config e
    ;  files := RelationFileAid.add fname entries (files e)
    ;  keys := keys e
    |}.
Definition env_add_aid_key (e : environment) (aid : string) (ki : keyinformation) : environment :=
    {| cur_assembly := cur_assembly e
    ;  cur_buffer := cur_buffer e
    ;  config := config e
    ;  files := files e
    ;  keys := RelationAidKey.add aid ki (keys e)
    |}.

End Environment.