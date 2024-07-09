(**
      e L y K s e e R
*)

Require Import Program.
Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String Structures.OrderedTypeEx FSets.FMapList Lia.

From LXR Require Import Cstdio Assembly Configuration Conversion Filesystem HList Nchunks Utilities.

Import ListNotations.

Module Export Distribution.

(*
            ┌───────────┐     ┌───────────┐   
            │           │     │           │   
            │   sink    ├─────┤   creds   │   
            │           │     │           │   
            └─────┬─────┘     └───────────┘   
                  │                           
                  │                           
     ┌────────────┴───┬───────────────┐       
     │                │               │       
┌────┴──────┐   ┌─────┴─────┐   ┌─────┴─────┐ 
│           │   │           │   │           │ 
│  s3sink   │   │  fssink   │   │  davsink  │ 
│           │   │           │   │           │ 
└───────────┘   └───────────┘   └───────────┘ 
                                              
*)

(*
  run: dune exec bin/lxr_distribute.exe -- -v -n 16 -x ../elykseer.chunks -i $MYID -a $AID -c sinks.json 4 8
*)

(* the configuration json

{
  "version": "1.0.0",
  "sinks": [
    {
        "type": "S3",
        "name": "s3_minio",
        "description": "minio storage cluster",
        "credentials": {
            "access-key": "01234-56789",
            "secret-key": "s3cr3t"
        },
        "access": {
            "bucket": "test",
            "host": "localhost",
            "port": "9000",
            "protocol": "http"
        }
    },
    {
        "type": "FS",
        "name": "fs_copy",
        "description": "filesystem copy",
        "credentials": {
            "user": "*",
            "group": "root",
            "permissions": "640"
        },
        "access": {
            "basepath": "/data/secure_stick"
        }
    }
  ]
}

*)

Definition ChunkId := String.

Module Export SMap := FMapList.Make String_as_OT.
(* Print SMap. *)

(* Module Example.
Definition testMap := SMap.add "key" "test"
                           (SMap.empty string).

Compute SMap.mem "key" testMap.  (* => true *) 
Compute SMap.mem "some" testMap.  (* => false *) 
Compute SMap.find "key" testMap.  (* => Some "test" *) 
Compute SMap.find "any" testMap.  (* => None *) 
End Example. *)

Definition RecordSink := Type.
Record sink (CONN : Type) (CREDS : Type) : RecordSink :=
    mksink {
        name : string;
        creds : CREDS;
        connection : CONN
    }.

Module Type SINK.
    (* Parameter Sink : RecordSink. *)
    Parameter K : Type.
    Definition B : Type := BufferEncrypted.buffer_t.
    Parameter CONN : Type.
    Parameter CREDS : Type.
    Definition Sink : RecordSink := sink CONN CREDS.
    Parameter init : configuration -> SMap.t K -> option Sink.
    Parameter push : K -> B -> Sink -> Sink.
    Parameter pull : K -> Sink -> (Sink * option B).
    Parameter list_n : Sink -> (Sink * list K).
End SINK.

(* Filesystem (local) Sink *)
Record fssink : Type :=
    mkfssink {
        fsbasepath: string;
    }.
Record fscredentials : Type :=
    mkfscredentials {
        fsuser: string;
    }.

Module FSSink <: SINK.
    Definition K := string.
    Definition B : Type := BufferEncrypted.buffer_t.
    Definition CONN := fssink.
    Definition CREDS := fscredentials.
    Definition Sink : RecordSink := sink CONN CREDS.
    Axiom fspush : K -> B -> Sink -> Sink.
    Axiom fspull : K -> Sink -> (Sink * option B).
    Axiom fslist_n : Sink -> (Sink * list K).
    Definition init (c : configuration) (s : SMap.t K) : option Sink :=
        match SMap.find "basepath" s with
        | None => None
        | Some p => match SMap.find "user" s with
            | None => None
            | Some u =>
                let creds := {| fsuser := u |} in
                let conn := {| fsbasepath := p |} in
                match SMap.find "name" s with
                | None => None
                | Some n => Some {| name := n; creds := creds; connection := conn |}
                end
            end
        end.
    Definition push : K -> B -> Sink -> Sink := fspush.
    Definition pull : K -> Sink -> (Sink * option B) := fspull.
    Definition list_n : Sink -> (Sink * list K) := fslist_n.
End FSSink.
(* Print FSSink. *)

(* S3 Sink *)
Record s3sink : Type :=
    mks3sink {
        s3protocol: string;
        s3host: string;
        s3port: string;
        s3bucket : string;
    }.
Record s3credentials : Type :=
    mks3credentials {
        s3user: string;
        s3password: string;
    }.

Module S3Sink <: SINK.
    Definition K := string.
    Definition B : Type := BufferEncrypted.buffer_t.
    Definition CONN := s3sink.
    Definition CREDS := s3credentials.
    Definition Sink : RecordSink := sink CONN CREDS.
    Axiom s3push : K -> B -> Sink -> Sink.
    Axiom s3pull : K -> Sink -> (Sink * option B).
    Axiom s3list_n : Sink -> (Sink * list K).
    Definition init (c : configuration) (s : SMap.t K) : option Sink :=
        match SMap.find "access" s with
        | None => None
        | Some u => match SMap.find "secret" s with
            | None => None
            | Some p => let creds := {| s3user := u; s3password := p |} in
                match SMap.find "bucket" s with
                | None => None
                | Some b => match SMap.find "host" s with
                    | None => None
                    | Some h => match SMap.find "protocol" s with
                        | None => None
                        | Some pr => match SMap.find "port" s with
                            | None => None
                            | Some p => let s3 := {| s3protocol := pr; s3host := h; s3port := p; s3bucket := b |} in
                                match SMap.find "name" s with
                                | None => None
                                | Some n => Some {| name := n; creds := creds; connection := s3 |}
                                end
                            end
                        end
                    end
                end
            end
        end.
    Definition push : K -> B -> Sink -> Sink := s3push.
    Definition pull : K -> Sink -> (Sink * option B) := s3pull.
    Definition list_n : Sink -> (Sink * list K) := s3list_n.
End S3Sink.
(* Print S3Sink. *)

Section Typing.

Inductive sink_type : Type :=
    | S3: S3Sink.Sink -> sink_type
    | FS: FSSink.Sink -> sink_type.

(* Definition get_name (os : option sink_type) : string :=
    match os with
    | None => "none"
    | Some st => match st with
        | S3 s => sink.name s
        | FS s => sink.name s
        | _ => "none"
        end
    end. *)

End Typing.

Section Distribute.

Definition enumerate_chunk_paths (c : configuration) (aid : aid_t) (nchunks : Nchunks.t) : list string :=
    let n := Nchunks.to_positive nchunks in
    List.fold_left (fun acc cid => chunk_identifier_path c aid cid :: acc)
        (Utilities.make_list n)
        [].

(* weights should be 0..nchunks *)
Definition distribute_by_weight (fps : list string) (ws0 : list N) : list (list string) :=
    let n := N.of_nat (List.length fps) in
    (* clip weights to length of file list *)
    let ws := List.map (fun w => if N.ltb n w then n else w) ws0 in
    (* repeat potential file list by how many selections *)
    let repfps := List.fold_left (fun acc _w => List.app fps acc) ws [] in
    let '(_, lls) := List.fold_left (fun '(fps, acc) w => let w' := N.to_nat w in (drop_list w' fps, (take_list w' fps) :: acc)) ws (repfps, []) in
    let lls' := List.map (fun ls => List.rev ls) lls in
    List.rev lls'.

(* Compute distribute_by_weight ["one";"two";"three"] [3%N;4%N]. *)

End Distribute.

Section Testing.

Example find_entry_in_empty : forall c,
    let s3params := ((SMap.add "name" "s3store")
                     ∘ (SMap.add "access" "s3user")
                     ∘ (SMap.add "secret" "s3secret")
                     ∘ (SMap.add "bucket" "test")
                     ∘ (SMap.add "host" "localhost")
                     ∘ (SMap.add "port" "9000"))
                    (SMap.empty string) in
    let fsparams := List.fold_left (fun acc '(k,v) => SMap.add k v acc)
                        [("name","filestore");("user","root");("basepath","/tmp/lxr")]
                        (SMap.empty string) in
    let distribution := ( (S3Sink.init c s3params)
                        , (FSSink.init c fsparams) ) in
    fst distribution <> None.
Proof.
    intros. simpl.
    Abort.
End Testing.

End Distribution.