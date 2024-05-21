(**
      e L y K s e e R
*)

Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String Program.Basics.

From LXR Require Import Assembly.
From LXR Require Import Configuration.
From LXR Require Import Conversion.
From LXR Require Import Cstdio.
From LXR Require Import Nchunks.
From LXR Require Import Store.
From LXR Require Import Tracer.

Module Export Environment.

Open Scope N_scope.


Definition RecordEnvironment := Type.
Record environment (AB : Type) : RecordEnvironment :=
    mkenvironment
        { cur_assembly : assemblyinformation
        ; cur_buffer : AB
        ; econfig : configuration
        }.

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
         ; cur_buffer := b
         ; econfig := c
        |}.
    Definition recreate_assembly (e : environment AB) : environment AB :=
        let (a,b) := AssemblyPlainWritable.create (e.(econfig AB)) in
        {| cur_assembly := a; cur_buffer := b
        ;  econfig := e.(econfig AB)
        |}.
    Definition finalise_assembly (e0 : environment AB) : option (aid_t * keyinformation) :=
        let a0 := e0.(cur_assembly AB) in
        let apos := Assembly.apos a0 in
        conditionalTrace e0.(econfig AB).(trace) (Tracer.info) (N.ltb 16 apos) (* apos > 16 *)
        (Some ("finalising assembly " ++ a0.(aid) ++ " with apos = " ++ i2s (n2i apos))%string)
        (fun _ =>
            let (a,b) := Assembly.finish a0 e0.(cur_buffer AB) in
            let ki := {| pkey := cpp_mk_key256 tt
                       ; ivec := cpp_mk_key128 tt
                       ; localnchunks := e0.(econfig AB).(Configuration.config_nchunks)
                       ; localid := e0.(econfig AB).(Configuration.my_id) |} in
            optionalTrace e0.(econfig AB).(trace) (Assembly.encrypt a b ki)
            (Tracer.warning) (Some ("failed to encrypt assembly: " ++ a.(aid))%string)
            (fun _ => None)
            (Tracer.info) (Some ("encrypted assembly: " ++ a.(aid))%string)
            (fun '(a',b') =>
                let n := Assembly.extract e0.(econfig AB) a' b' in
                if N.ltb 0 n
                then Some (a.(aid), ki)
                else None
            )
        )
        (Some ("not finalising empty assembly " ++ a0.(aid) ++ " with apos = "  ++ i2s (n2i apos))%string)
        (fun _ => None).
    
    Definition finalise_and_recreate_assembly (e0 : environment AB) : option (environment AB * (aid_t * keyinformation)) :=
        match finalise_assembly e0 with
        | None => None
        | Some ki =>
            Some (EnvironmentWritable.recreate_assembly e0, ki)
        end.
    
    Program Definition backup (e0 : environment AB) (fp : string) (fpos : N) (content : BufferPlain.buffer_t) : (environment AB * (blockinformation * option (aid_t * keyinformation))) :=
        let afree := (Assembly.assemblysize e0.(econfig AB).(Configuration.config_nchunks)) - e0.(cur_assembly AB).(apos) in
        let blen := BufferPlain.buffer_len content in
        let (ki, e1) := if afree <? blen then
                            match finalise_and_recreate_assembly e0 with
                            | None => (None, e0)
                            | Some (e0', ki') => (Some ki', e0')
                            end
                        else (None, e0) in
        let (a', bi) := Assembly.backup e1.(cur_assembly AB) e1.(cur_buffer AB) fpos content in
        ({| cur_assembly := a'
          ; cur_buffer := e1.(cur_buffer AB)
          ; econfig := e1.(econfig AB)
         |}, (bi, ki)).

End EnvironmentWritable.
(* Print EnvironmentWritable. *)

Module EnvironmentReadable <: ENV.
    Definition AB := AssemblyPlainFull.B.
    Definition E : RecordEnvironment := environment AB.
    Definition initial_environment (c : configuration) : E :=
        let (a,b) := AssemblyPlainFull.create c in
        {| cur_assembly := a
         ; cur_buffer := b
         ; econfig := c
        |}.
    Definition restore_assembly (e0 : environment AB) (aid : aid_t) (ki : keyinformation) : option (environment AB) :=
        match Assembly.recall e0.(econfig AB) {| nchunks := e0.(econfig AB).(Configuration.config_nchunks); aid := aid; apos := 0 |} with
        | None => None
        | Some (a1, b1) =>
            match Assembly.decrypt a1 b1 ki with
            | None => None
            | Some (a2, b2) =>
                Some {| cur_assembly := a2; cur_buffer := b2; econfig := e0.(econfig AB) |}
            end
        end.

End EnvironmentReadable.
(* Print EnvironmentReadable. *)

End Environment.