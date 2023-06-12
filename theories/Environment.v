(**
      e L y K s e e R
*)

Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String Program.Basics.

From RecordUpdate Require Import RecordUpdate.

From LXR Require Import Assembly.
From LXR Require Import Buffer.
From LXR Require Import Configuration.
From LXR Require Import Nchunks.

Module Export Environment.

(* Open Scope positive_scope. *)
Open Scope N_scope.

Record environment : Type :=
    mkenvironment
        { cur_assembly : AssemblyPlainWritable.H
        ; cur_buffer : AssemblyPlainWritable.B
        ; config : configuration
        ; fblocks : list (string * Assembly.blockinformation)
        ; keys : list (string * Assembly.keyinformation)
        }.

#[export] Instance etaX : Settable _ := settable! mkenvironment <cur_assembly; cur_buffer; config; fblocks; keys>.

Definition initial_environment (c : configuration) : environment :=
    let (a,b) := AssemblyPlainWritable.create c in
    {| cur_assembly := a
    ;  cur_buffer := b
    ;  config := c
    ;  fblocks := nil
    ;  keys := nil
    |}.

Definition recreate_assembly (e : environment) : environment :=
    let (a,b) := AssemblyPlainWritable.create e.(config) in
    set cur_assembly (const a) (set cur_buffer (const b) e).

Definition env_add_file_block (fname : string) (e : environment) (bi : Assembly.blockinformation) : environment :=
    set fblocks (fun bs => (fname,bi) :: bs) e.

Definition env_add_aid_key (aid : string) (e : environment) (ki : keyinformation) : environment :=
    set keys (fun ks => (aid,ki) :: ks) e.

(* find a key for an aid *)
Definition key_for_aid (e : environment) (aid : Assembly.aid_t) : option keyinformation :=
    match List.filter (fun e => String.eqb (fst e) aid) e.(keys) with
    | nil => None
    | (_,ki) :: _ => Some ki
    end.

(* the decryption key needs to be preloaded in the key list
   TODO: pass the key in
 *)
Definition restore_assembly (e0 : environment) (aid : Assembly.aid_t) : option environment :=
    match key_for_aid e0 aid with
    | None => None
    | Some k =>
        match Assembly.recall e0.(config) {| nchunks := e0.(config).(Configuration.config_nchunks); aid := aid; apos := 0 |} with
        | None => None
        | Some (a1, b1) =>
            match Assembly.decrypt a1 b1 k with
            | None => None
            | Some (a2, b2) =>
                Some {| cur_assembly := a2; cur_buffer := b2; config := e0.(config); fblocks := e0.(fblocks); keys := e0.(keys) |}
            end
        end
    end.

Axiom cpp_mk_key256 : unit -> string.
Axiom cpp_mk_key128 : unit -> string.
Definition finalise_assembly (e0 : environment) : environment :=
    let a0 := cur_assembly e0 in
    let apos := apos a0 in
    if N.ltb 0 apos then
        let (a,b) := Assembly.finish a0 (cur_buffer e0) in
        let ki := {| pkey := cpp_mk_key256 tt
                   ; ivec := cpp_mk_key128 tt
                   ; localnchunks := e0.(config).(Configuration.config_nchunks)
                   ; localid := e0.(config).(Configuration.my_id) |} in
        let e1 := env_add_aid_key (aid a) e0 ki in
        match Assembly.encrypt a b ki with
        | None => e0
        | Some (a',b') =>
            let n := Assembly.extract (config e1) a' b' in
            if N.eqb n (Assembly.assemblysize e0.(config).(Configuration.config_nchunks))
            then e1
            else e0
        end
    else e0.
  
Definition finalise_and_recreate_assembly (e0 : environment) : environment :=
    let e1 := finalise_assembly e0 in
    recreate_assembly e1.
  
Program Definition backup (e0 : environment) (fp : string) (fpos : N) (content : BufferPlain.buffer_t) : environment :=
    let afree := N.sub (Assembly.assemblysize e0.(config).(Configuration.config_nchunks)) e0.(cur_assembly).(apos) in
    let blen := BufferPlain.buffer_len content in
    let e1 := if N.ltb afree blen then
                finalise_and_recreate_assembly e0
              else e0 in
    let (a', bi) := Assembly.backup e1.(cur_assembly) e1.(cur_buffer) fpos content in
    {| cur_assembly := a'
    ;  cur_buffer := e1.(cur_buffer)
    ;  config := e1.(config)
    ;  fblocks := (fp,bi) :: e1.(fblocks)
    ;  keys := e1.(keys)
    |}.

End Environment.