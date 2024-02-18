(**
      e L y K s e e R
*)

Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String Program.Basics.

From RecordUpdate Require Import RecordUpdate.

From LXR Require Import Assembly.
From LXR Require Import Configuration.
From LXR Require Import Cstdio.
From LXR Require Import Store.
From LXR Require Import Nchunks.

Module Export Environment.

Open Scope N_scope.


Definition RecordEnvironment := Type.
Record environment (AB : Type) : RecordEnvironment :=
    mkenvironment
        { cur_assembly : assemblyinformation
        ; cur_buffer : AB
        ; config : configuration
        ; fblocks : FBlockListStore.R
        ; keys : KeyListStore.R
        }.
(* #[export] Instance etaX : Settable _ := settable! mkenvironment (AB) <cur_assembly; cur_buffer; config; fblocks; keys>. *)
(* Print environment.
Print cur_assembly.
Print set.
Print Setter.  *)

Axiom cpp_mk_key256 : unit -> string.
Axiom cpp_mk_key128 : unit -> string.

Module Type ENV.
    Parameter AB : Type.
    Definition E : RecordEnvironment := environment AB.
    Axiom initial_environment : configuration -> E.
End ENV.
(* Print ENV. *)

Module EnvironmentWritable <: ENV.
    Definition AB := AssemblyPlainWritable.B.
    Definition E : RecordEnvironment := environment AB.
    Definition initial_environment (c : configuration) : E :=
        let (a,b) := AssemblyPlainWritable.create c in
        {| cur_assembly := a
        ;  cur_buffer := b
        ;  config := c
        ;  fblocks := FBlockListStore.init c
        ;  keys := KeyListStore.init c
        |}.
    Definition recreate_assembly (e : environment AB) : environment AB :=
        let (a,b) := AssemblyPlainWritable.create (e.(config AB)) in
        {| cur_assembly := a; cur_buffer := b
        ;  config := e.(config AB)
        ;  fblocks := e.(fblocks AB)
        ;  keys := e.(keys AB)
        |}.
    Definition env_add_file_block (fname : string) (e : environment AB) (bi : Assembly.blockinformation) : environment AB :=
        {| cur_assembly := e.(cur_assembly AB)
        ;  cur_buffer := e.(cur_buffer AB)
        ;  config := e.(config AB)
        ;  fblocks := FBlockListStore.add fname bi e.(fblocks AB)
        ;  keys := e.(keys AB)
        |}.
    Definition env_add_aid_key (aid : aid_t) (e : environment AB) (ki : keyinformation) : environment AB :=
        {| cur_assembly := e.(cur_assembly AB)
        ;  cur_buffer := e.(cur_buffer AB)
        ;  config := e.(config AB)
        ;  fblocks := e.(fblocks AB)
        ;  keys := KeyListStore.add aid ki e.(keys AB)
        |}.
    Definition key_for_aid (e : environment AB) (aid : Assembly.aid_t) : option keyinformation :=
        KeyListStore.find aid e.(keys AB).
    Definition finalise_assembly (e0 : environment AB) : environment AB :=
        let a0 := e0.(cur_assembly AB) in
        let apos := Assembly.apos a0 in
        if N.ltb 0 apos then
            let (a,b) := Assembly.finish a0 e0.(cur_buffer AB) in
            let ki := {| pkey := cpp_mk_key256 tt
                       ; ivec := cpp_mk_key128 tt
                       ; localnchunks := e0.(config AB).(Configuration.config_nchunks)
                       ; localid := e0.(config AB).(Configuration.my_id) |} in
            let e1 := env_add_aid_key (aid a) e0 ki in
            match Assembly.encrypt a b ki with
            | None => e0
            | Some (a',b') =>
                let n := Assembly.extract e1.(config AB) a' b' in
                if N.ltb 0 n
                then e1
                else e0
            end
        else e0.
    
    Definition finalise_and_recreate_assembly (e0 : environment AB) : environment AB :=
        let e1 := finalise_assembly e0 in
        EnvironmentWritable.recreate_assembly e1.
    
    Program Definition backup (e0 : environment AB) (fp : string) (fpos : N) (content : BufferPlain.buffer_t) : (N * environment AB) :=
        let afree := N.sub (Assembly.assemblysize e0.(config AB).(Configuration.config_nchunks)) e0.(cur_assembly AB).(apos) in
        let blen := BufferPlain.buffer_len content in
        let e1 := if N.ltb afree blen then
                    finalise_and_recreate_assembly e0
                else e0 in
        let (a', bi) := Assembly.backup e1.(cur_assembly AB) e1.(cur_buffer AB) fpos content in
        let apos := bi.(blockapos) in
        (apos, {| cur_assembly := a'
                ; cur_buffer := e1.(cur_buffer AB)
                ; config := e1.(config AB)
                ; fblocks := FBlockListStore.add fp bi e1.(fblocks AB)
                ; keys := e1.(keys AB)
               |}).

End EnvironmentWritable.
(* Print EnvironmentWritable. *)

Module EnvironmentReadable <: ENV.
    Definition AB := AssemblyPlainFull.B.
    Definition E : RecordEnvironment := environment AB.
    Definition initial_environment (c : configuration) : E :=
        let (a,b) := AssemblyPlainFull.create c in
        {| cur_assembly := a
        ;  cur_buffer := b
        ;  config := c
        ;  fblocks := FBlockListStore.init c
        ;  keys := KeyListStore.init c
        |}.
    Definition env_add_aid_key (aid : aid_t) (e : environment AB) (ki : keyinformation) : environment AB :=
        {| cur_assembly := e.(cur_assembly AB)
        ;  cur_buffer := e.(cur_buffer AB)
        ;  config := e.(config AB)
        ;  fblocks := e.(fblocks AB)
        ;  keys := KeyListStore.add aid ki e.(keys AB)
        |}.
    Definition key_for_aid (e : environment AB) (aid : Assembly.aid_t) : option keyinformation :=
        KeyListStore.find aid e.(keys AB).
    Definition restore_assembly (e0 : environment AB) (aid : aid_t) : option (environment AB) :=
        match key_for_aid e0 aid with
        | None => None
        | Some k =>
            match Assembly.recall e0.(config AB) {| nchunks := e0.(config AB).(Configuration.config_nchunks); aid := aid; apos := 0 |} with
            | None => None
            | Some (a1, b1) =>
                match Assembly.decrypt a1 b1 k with
                | None => None
                | Some (a2, b2) =>
                    Some {| cur_assembly := a2; cur_buffer := b2; config := e0.(config AB); fblocks := e0.(fblocks AB); keys := e0.(keys AB) |}
                end
            end
        end.

End EnvironmentReadable.
(* Print EnvironmentReadable. *)

End Environment.