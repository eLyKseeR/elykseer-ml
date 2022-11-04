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
Definition env_add_file_block (fname : string) (e : environment) (bi : blockinformation) : environment :=
    let mkbi bi bis :=
        let newblockid bis :=
            match bis with
            | nil => 1%positive
            | bi :: _ => (1 + blockid bi)%positive
            end in
        {| blockid := newblockid bis; bchecksum := bchecksum bi; blocksize := blocksize bi;
           filepos := filepos bi; blockaid := blockaid bi; blockapos := blockapos bi
        |} in
    let entries :=
        match RelationFileAid.find fname (files e) with
        | Some bis => (mkbi bi bis) :: bis
        | None => (mkbi bi nil) :: nil
        end in
    {| cur_assembly := cur_assembly e
    ;  cur_buffer := cur_buffer e
    ;  config := config e
    ;  files := RelationFileAid.add fname entries (files e)
    ;  keys := keys e
    |}.
Definition env_add_aid_key (aid : string) (e : environment) (ki : keyinformation) : environment :=
    {| cur_assembly := cur_assembly e
    ;  cur_buffer := cur_buffer e
    ;  config := config e
    ;  files := files e
    ;  keys := RelationAidKey.add aid ki (keys e)
    |}.

Program Definition backup (e0 : environment) (fp : string) (fpos : N) (content : BufferPlain.buffer_t) : environment :=
    let (a', bi) := Assembly.backup (cur_assembly e0) (cur_buffer e0) fpos content in
    let e1 := env_add_file_block fp e0 bi in
    {| cur_assembly := a'
    ;  cur_buffer := cur_buffer e1
    ;  config := config e1
    ;  files := files e1
    ;  keys := keys e1
    |}.

End Environment.