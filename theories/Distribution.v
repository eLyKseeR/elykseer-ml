(**
      e L y K s e e R
*)

From Stdlib Require Import Program.
From Stdlib Require Import ZArith NArith PArith.
From Stdlib Require Import NArith.BinNat Lists.List Sorting.Permutation Strings.String Structures.OrderedTypeEx FSets.FMapList Lia.

From LXR Require Import Cstdio Assembly Configuration Filesystem Nchunks Utilities.

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
│  s3sink   │   │  fssink   │   │ miniosink │ 
│           │   │           │   │           │ 
└───────────┘   └───────────┘   └───────────┘ 
                                              
*)

(*
  run: dune exec bin/lxr_distribute.exe -- -v -d PUT -n 16 -x ../elykseer.chunks -i $MYID -a $AID -c sinks.json 4 8

  the first sink receives the first 4 chunks, the second one the next 8 chunks

  layers:
  - per sink: the low-level access is axiomatic (Filesystem.v for FS, s3_* for S3);
    the decisions (already present, size checks, which codes are transient) are Coq code
  - plan: which chunk goes to which sink (pure, proved)
  - schedule: tries every planned chunk, retries transient failures with exponential
    backoff (pure, parameterised over the copy function and the clock, proved)
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
            "prefix": "lxr",
            "host": "localhost",
            "port": "9000",
            "protocol": "https"
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
        },
        "retry": { "max_attempts": 2, "base_delay_ms": 100, "max_delay_ms": 1000, "deadline_ms": 10000 }
    }
  ]
}

*)

Open Scope string_scope.
Open Scope nat_scope.
Open Scope bool_scope.

Module Export SMap := FMapList.Make String_as_OT.

(** * Results and retry policy *)

Inductive copy_result : Type :=
    | Copied                (* the chunk was copied *)
    | Present               (* the chunk was already at the target *)
    | Retry (code : Z)      (* transient failure, worth another attempt *)
    | Failed (code : Z).    (* permanent failure *)

Definition is_ok (r : copy_result) : bool :=
    match r with Copied | Present => true | _ => false end.

Inductive direction : Type := PUT | GET.

Record retrypolicy : Type :=
    mkretrypolicy {
        max_attempts : nat;     (* at least one attempt is always made *)
        base_delay_ms : N;
        max_delay_ms : N;
        deadline_ms : N         (* time budget per chunk, from its first attempt *)
    }.

(* the size of a chunk file *)
Definition chunk_size : N := Assembly.chunksize_N.

(** * Sinks *)

Definition RecordSink := Type.
Record sink (CONN : Type) (CREDS : Type) : RecordSink :=
    mksink {
        name : string;
        creds : CREDS;
        connection : CONN;
        localchunks : string;   (* the local chunk directory; a key K resolves to localchunks/K *)
        retry : retrypolicy
    }.
Arguments name {CONN CREDS}.
Arguments creds {CONN CREDS}.
Arguments connection {CONN CREDS}.
Arguments localchunks {CONN CREDS}.
Arguments retry {CONN CREDS}.

Module Type SINK.
    (* chunk key relative to the chunk path: "ab/cd/<cid>.lxr" *)
    Definition K := string.
    Parameter CONN : Type.
    Parameter CREDS : Type.
    Definition Sink : RecordSink := sink CONN CREDS.
    Parameter init : configuration -> SMap.t string -> option Sink.
    (* local chunk -> sink *)
    Parameter put : K -> Sink -> (Sink * copy_result).
    (* sink -> local chunk *)
    Parameter get : K -> Sink -> (Sink * copy_result).
    (* up to n randomly chosen chunks held by the sink *)
    Parameter list_n : N -> Sink -> (Sink * list K).
End SINK.

(* local failure codes; below the negated curl error codes (-1 .. -99) that S3 returns *)
Definition no_sink_dir : Z := (-1001)%Z.
Definition copy_failed : Z := (-1002)%Z.
Definition wrong_size : Z := (-1003)%Z.
Definition mkdir_failed : Z := (-1004)%Z.
Definition no_source : Z := (-1005)%Z.
Definition remove_failed : Z := (-1006)%Z.

(* note on the theorems per sink: file_exists, file_size, .. are pure functions here, so
   they state which checks guard a result, not how the disk changed between two calls *)

Definition local_path {C D : Type} (s : sink C D) (k : string) : string :=
    s.(localchunks) ++ "/" ++ k.

(* only check the size of an existing file: file_size of a missing one is undefined *)
Definition is_chunk_file (p : Filesystem.path) : bool :=
    Filesystem.Path.file_exists p && N.eqb (Filesystem.Path.file_size p) chunk_size.

(* a file of the wrong size (an interrupted copy) is in the way: remove it *)
Definition clear_partial (p : Filesystem.path) : bool :=
    negb (Filesystem.Path.file_exists p) || Filesystem.remove p.

Definition ensure_parent (p : Filesystem.path) : bool :=
    let dir := Filesystem.Path.parent p in
    Filesystem.Path.is_directory dir || Filesystem.create_directories dir.

(* picks up to n elements of ls; rand i gives the i-th random number *)
Fixpoint sample {A : Set} (rand : N -> N) (n : nat) (ls : list A) : list A :=
    match n with
    | O => []
    | S n' =>
        match ls with
        | [] => []
        | x0 :: _ =>
            let i := N.to_nat (N.modulo (rand (N.of_nat n)) (N.of_nat (List.length ls))) in
            nth i ls x0 :: sample rand n' (take_list i ls ++ drop_list (S i) ls)
        end
    end.

Lemma sample_in : forall (A : Set) rand n (ls : list A) x,
    List.In x (sample rand n ls) -> List.In x ls.
Proof.
    intros A rand n; induction n; intros ls x H; simpl in H; [contradiction|].
    destruct ls as [|x0 r]; [contradiction|].
    destruct H as [H|H].
    - subst. apply nth_In.
      set (L := List.length (x0 :: r)).
      assert (HL : 0 < L) by (unfold L; simpl; lia).
      match goal with |- N.to_nat ?x < L =>
        assert (Hlt : (x < N.of_nat L)%N) by (apply N.mod_lt; lia);
        revert Hlt; generalize x; clearbody L; intros; lia end.
    - apply IHn in H. apply in_app_or in H. destruct H as [H|H].
      + eapply take_list_in; eauto.
      + eapply drop_list_in; eauto.
Qed.

(* Filesystem (local) Sink *)
Record fssink : Type :=
    mkfssink {
        fsbasepath: string;
    }.
Record fscredentials : Type :=
    mkfscredentials {
        fsuser: string;
    }.

Definition fs_default_retry : retrypolicy :=
    {| max_attempts := 2; base_delay_ms := 100; max_delay_ms := 1000; deadline_ms := 10000 |}.

Module FSSink <: SINK.
    Definition K := string.
    Definition CONN := fssink.
    Definition CREDS := fscredentials.
    Definition Sink : RecordSink := sink CONN CREDS.
    Definition init (c : configuration) (s : SMap.t string) : option Sink :=
        match SMap.find "basepath" s with
        | None => None
        | Some p => match SMap.find "user" s with
            | None => None
            | Some u =>
                let creds := {| fsuser := u |} in
                let conn := {| fsbasepath := p |} in
                match SMap.find "name" s with
                | None => None
                | Some n => Some {| name := n; creds := creds; connection := conn;
                                    localchunks := c.(path_chunks); retry := fs_default_retry |}
                end
            end
        end.

    Definition sink_path (s : Sink) (k : K) : string :=
        s.(connection).(fsbasepath) ++ "/" ++ k.

    (* a missing sink directory is not created: it may be an unmounted stick *)
    Definition put (k : K) (s : Sink) : (Sink * copy_result) :=
        let base := Filesystem.Path.from_string s.(connection).(fsbasepath) in
        let src := Filesystem.Path.from_string (local_path s k) in
        let tgt := Filesystem.Path.from_string (sink_path s k) in
        (s, if negb (Filesystem.Path.is_directory base) then Failed no_sink_dir
            else if is_chunk_file tgt then Present
            else if negb (is_chunk_file src) then Failed wrong_size
            else if negb (clear_partial tgt) then Failed remove_failed
            else if negb (ensure_parent tgt) then Retry mkdir_failed
            else if Filesystem.copy_file src tgt then Copied
            else Retry copy_failed).

    Definition get (k : K) (s : Sink) : (Sink * copy_result) :=
        let base := Filesystem.Path.from_string s.(connection).(fsbasepath) in
        let src := Filesystem.Path.from_string (sink_path s k) in
        let tgt := Filesystem.Path.from_string (local_path s k) in
        (* a chunk that is already here needs no sink: another sink may have delivered it *)
        (s, if is_chunk_file tgt then Present
            else if negb (Filesystem.Path.is_directory base) then Failed no_sink_dir
            else if negb (Filesystem.Path.file_exists src) then Failed no_source
            else if negb (clear_partial tgt) then Failed remove_failed
            else if negb (ensure_parent tgt) then Retry mkdir_failed
            else if negb (Filesystem.copy_file src tgt) then Retry copy_failed
            else if N.eqb (Filesystem.Path.file_size tgt) chunk_size then Copied
            (* do not leave a copy of the wrong size behind *)
            else if Filesystem.remove tgt then Failed wrong_size
            else Failed remove_failed).

    (* the keys "ab/cd/<cid>.lxr" in the sink directory *)
    Definition list_keys (s : Sink) : list K :=
        let fname de := Filesystem.Path.to_string (Filesystem.Path.filename (Filesystem.Direntry.as_path de)) in
        let base := Filesystem.Path.from_string s.(connection).(fsbasepath) in
        Filesystem.list_directory base [] (fun d1 acc1 =>
            if negb (Filesystem.Direntry.is_directory d1) then acc1 else
            Filesystem.list_directory (Filesystem.Direntry.as_path d1) acc1 (fun d2 acc2 =>
                if negb (Filesystem.Direntry.is_directory d2) then acc2 else
                Filesystem.list_directory (Filesystem.Direntry.as_path d2) acc2 (fun d3 acc3 =>
                    if Filesystem.Direntry.is_regular_file d3
                    then (fname d1 ++ "/" ++ fname d2 ++ "/" ++ fname d3) :: acc3
                    else acc3))).

    Definition list_n (n : N) (s : Sink) : (Sink * list K) :=
        (s, sample Utilities.rnd (N.to_nat n) (list_keys s)).

    Theorem put_copied_is_chunk : forall k s,
        snd (put k s) = Copied -> is_chunk_file (Filesystem.Path.from_string (local_path s k)) = true.
    Proof.
        intros k s; unfold put; simpl.
        repeat match goal with |- context [if ?b then _ else _] => destruct b eqn:? end;
          simpl; intro H; try discriminate; auto.
        all: match goal with H : negb ?b = false |- ?b = true => destruct b; [reflexivity | discriminate] end.
    Qed.

    (* only a full-size chunk on the sink counts as present *)
    Theorem put_present_is_chunk : forall k s,
        snd (put k s) = Present -> is_chunk_file (Filesystem.Path.from_string (sink_path s k)) = true.
    Proof.
        intros k s; unfold put; simpl.
        repeat match goal with |- context [if ?b then _ else _] => destruct b eqn:? end;
          simpl; intro H; try discriminate; auto.
    Qed.

    Theorem get_copied_size : forall k s,
        snd (get k s) = Copied ->
        Filesystem.Path.file_size (Filesystem.Path.from_string (local_path s k)) = chunk_size.
    Proof.
        intros k s; unfold get; simpl.
        repeat match goal with |- context [if ?b then _ else _] => destruct b eqn:? end;
          simpl; intro H; try discriminate.
        apply N.eqb_eq; assumption.
    Qed.

    Theorem get_present_is_chunk : forall k s,
        snd (get k s) = Present -> is_chunk_file (Filesystem.Path.from_string (local_path s k)) = true.
    Proof.
        intros k s; unfold get; simpl.
        repeat match goal with |- context [if ?b then _ else _] => destruct b eqn:? end;
          simpl; intro H; try discriminate; auto.
    Qed.

    Theorem list_n_from_sink : forall n s k,
        List.In k (snd (list_n n s)) -> List.In k (list_keys s).
    Proof. intros n s k; apply sample_in. Qed.
End FSSink.

(* S3 Sink *)
Record s3sink : Type :=
    mks3sink {
        s3protocol: string;
        s3host: string;
        s3port: string;
        s3bucket : string;
        s3prefix : string;
    }.
Record s3credentials : Type :=
    mks3credentials {
        s3user: string;
        s3password: string;
    }.

(* low-level S3 access (elykseer-base/s3access.ml); return the HTTP status code,
   or a negated curl error code *)
Axiom s3_put_object : s3sink -> s3credentials -> string (* resource *) -> string (* local file *) -> Z.
(* writes the local file only on an HTTP success *)
Axiom s3_get_object : s3sink -> s3credentials -> string (* resource *) -> string (* local file *) -> Z.
(* the object names below the prefix *)
Axiom s3_list_objects : s3sink -> s3credentials -> string (* prefix *) -> list string.

(* curl errors, 5xx, 429 and 408 are transient; other codes outside 2xx are permanent *)
Definition s3_result (code : Z) : copy_result :=
    if (200 <=? code)%Z && (code <? 300)%Z then Copied
    else if (code <? 0)%Z || (500 <=? code)%Z || (code =? 429)%Z || (code =? 408)%Z then Retry code
    else Failed code.

Definition s3_default_retry : retrypolicy :=
    {| max_attempts := 5; base_delay_ms := 500; max_delay_ms := 30000; deadline_ms := 600000 |}.

Module S3Sink <: SINK.
    Definition K := string.
    Definition CONN := s3sink.
    Definition CREDS := s3credentials.
    Definition Sink : RecordSink := sink CONN CREDS.
    Definition init (c : configuration) (s : SMap.t string) : option Sink :=
        match SMap.find "access" s with
        | None => None
        | Some u => match SMap.find "secret" s with
            | None => None
            | Some p => let creds := {| s3user := u; s3password := p |} in
                match SMap.find "bucket" s with
                | None => None
                | Some b => match SMap.find "prefix" s with
                    | None => None
                    | Some prefix => match SMap.find "host" s with
                        | None => None
                        | Some h => match SMap.find "protocol" s with
                            | None => None
                            | Some prot => match SMap.find "port" s with
                                | None => None
                                | Some p => let s3 := {| s3protocol := prot; s3host := h; s3port := p; s3bucket := b; s3prefix := prefix |} in
                                    match SMap.find "name" s with
                                    | None => None
                                    | Some n => Some {| name := n; creds := creds; connection := s3;
                                                        localchunks := c.(path_chunks); retry := s3_default_retry |}
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end.

    Definition resource (s : Sink) (k : K) : string :=
        "/" ++ s.(connection).(s3prefix) ++ "/" ++ k.

    (* shortcut: no check whether the object is already there (needs a HEAD request); a PUT overwrites it with the same content *)
    Definition put (k : K) (s : Sink) : (Sink * copy_result) :=
        let lp := local_path s k in
        (s, if negb (is_chunk_file (Filesystem.Path.from_string lp)) then Failed wrong_size
            else s3_result (s3_put_object s.(connection) s.(creds) (resource s k) lp)).

    Definition get (k : K) (s : Sink) : (Sink * copy_result) :=
        let lp := local_path s k in
        let p := Filesystem.Path.from_string lp in
        (s, if is_chunk_file p then Present
            else if negb (clear_partial p) then Failed remove_failed
            else match s3_result (s3_get_object s.(connection) s.(creds) (resource s k) lp) with
                 | Copied =>
                     if N.eqb (Filesystem.Path.file_size p) chunk_size then Copied
                     (* a short download is transient; do not leave it behind *)
                     else if Filesystem.remove p then Retry copy_failed
                     else Failed remove_failed
                 | r => r
                 end).

    Definition list_n (n : N) (s : Sink) : (Sink * list K) :=
        (s, sample Utilities.rnd (N.to_nat n) (s3_list_objects s.(connection) s.(creds) s.(connection).(s3prefix))).

    Theorem put_copied_is_chunk : forall k s,
        snd (put k s) = Copied -> is_chunk_file (Filesystem.Path.from_string (local_path s k)) = true.
    Proof.
        intros k s; unfold put; simpl.
        destruct (is_chunk_file _); simpl; auto; discriminate.
    Qed.

    Theorem get_copied_size : forall k s,
        snd (get k s) = Copied ->
        Filesystem.Path.file_size (Filesystem.Path.from_string (local_path s k)) = chunk_size.
    Proof.
        intros k s; unfold get; simpl.
        destruct (is_chunk_file _); [discriminate|].
        destruct (clear_partial _); simpl; [|discriminate].
        destruct (s3_result _); try discriminate; auto.
        destruct (N.eqb _ _) eqn:E; [intros _; apply N.eqb_eq; assumption|].
        destruct (Filesystem.remove _); discriminate.
    Qed.
End S3Sink.

Section Typing.

Inductive sink_type : Type :=
    | S3:    S3Sink.Sink -> sink_type
    | FS:    FSSink.Sink -> sink_type.

End Typing.

(** * Plan: which chunk goes to which sink *)

Section Plan.
Open Scope list_scope.

(* the chunk key "ab/cd/<cid>.lxr" (same layout as chunk_identifier_path) *)
Definition chunk_key_of_cid (cid : string) : string :=
    let n := String.length cid in
    substring (n - 2) 2 cid ++ "/" ++ substring (n - 4) 2 cid ++ "/" ++ cid ++ ".lxr".

Definition enumerate_chunk_keys (c : configuration) (aid : aid_t) (nchunks : Nchunks.t) : list string :=
    List.map (fun cid => chunk_key_of_cid (chunk_identifier c aid cid))
             (Utilities.make_list (Nchunks.to_positive nchunks)).

Fixpoint distribute_aux {A : Set} (rep : list A) (ws : list nat) : list (list A) :=
    match ws with
    | [] => []
    | w :: ws' => take_list w rep :: distribute_aux (drop_list w rep) ws'
    end.

(* group i gets min w_i n consecutive keys of the keys repeated (a window on a ring):
   a sum of weights above n stores keys on several sinks *)
Definition distribute_by_weight {A : Set} (ks : list A) (ws0 : list N) : list (list A) :=
    let n := List.length ks in
    let ws := List.map (fun w => Nat.min (N.to_nat w) n) ws0 in
    distribute_aux (List.concat (List.repeat ks (List.length ws))) ws.

(* one weight per sink, and together they reach every chunk *)
Definition valid_weights (nsinks : nat) (n : N) (ws : list N) : bool :=
    Nat.eqb (List.length ws) nsinks && N.leb n (List.fold_right N.add 0%N ws).

Definition plan {S : Type} {A : Set} (sinks : list S) (ws : list N) (ks : list A) : option (list (S * list A)) :=
    if valid_weights (List.length sinks) (N.of_nat (List.length ks)) ws
    then Some (List.combine sinks (distribute_by_weight ks ws))
    else None.

Lemma distribute_aux_concat : forall (A : Set) ws (rep : list A),
    List.concat (distribute_aux rep ws) = take_list (List.list_sum ws) rep.
Proof.
    intros A ws; induction ws as [|w ws IH]; intro rep; simpl.
    - destruct rep; reflexivity.
    - rewrite IH, take_list_add; reflexivity.
Qed.

Lemma distribute_aux_sizes : forall (A : Set) n ws (rep : list A),
    (forall w, List.In w ws -> w <= n) -> n * List.length ws <= List.length rep ->
    List.map (@List.length A) (distribute_aux rep ws) = ws.
Proof.
    intros A n ws; induction ws as [|w ws IH]; intros rep Hw Hl; simpl; auto.
    simpl List.length in Hl. rewrite Nat.mul_succ_r in Hl.
    assert (w <= n) by (apply Hw; left; auto).
    rewrite take_list_length, IH.
    - f_equal. lia.
    - intros; apply Hw; right; auto.
    - rewrite drop_list_length. lia.
Qed.

Lemma distribute_aux_in : forall (A : Set) ws (rep : list A) g x,
    List.In g (distribute_aux rep ws) -> List.In x g -> List.In x rep.
Proof.
    intros A ws; induction ws as [|w ws IH]; intros rep g x Hg Hx; simpl in Hg; [contradiction|].
    destruct Hg as [Hg|Hg].
    - subst; eapply take_list_in; eauto.
    - eapply drop_list_in, IH; eauto.
Qed.

Lemma concat_repeat_length : forall (A : Set) (ks : list A) c,
    List.length (List.concat (List.repeat ks c)) = c * List.length ks.
Proof.
    intros A ks c; induction c; simpl; auto.
    rewrite length_app, IHc; reflexivity.
Qed.

Theorem distribute_group_sizes : forall (A : Set) (ks : list A) ws,
    List.map (@List.length A) (distribute_by_weight ks ws)
    = List.map (fun w => Nat.min (N.to_nat w) (List.length ks)) ws.
Proof.
    intros. unfold distribute_by_weight.
    apply distribute_aux_sizes with (n := List.length ks).
    - intros w Hw. apply in_map_iff in Hw. destruct Hw as [w' [<- _]]. lia.
    - rewrite concat_repeat_length. lia.
Qed.

Lemma sum_min_ge : forall n ws,
    Nat.min n (List.list_sum (List.map N.to_nat ws))
    <= List.list_sum (List.map (fun w => Nat.min (N.to_nat w) n) ws).
Proof.
    intros n ws; induction ws as [|w ws IH]; simpl; lia.
Qed.

Lemma sum_N_nat : forall ws,
    N.to_nat (List.fold_right N.add 0%N ws) = List.list_sum (List.map N.to_nat ws).
Proof.
    intros ws; induction ws as [|w ws IH]; simpl; auto.
    rewrite N2Nat.inj_add, IH; reflexivity.
Qed.

Theorem distribute_covers : forall (A : Set) (ks : list A) ws k,
    (N.of_nat (List.length ks) <= List.fold_right N.add 0%N ws)%N ->
    List.In k ks -> exists g, List.In g (distribute_by_weight ks ws) /\ List.In k g.
Proof.
    intros A ks ws k Hsum Hk.
    assert (Hsum' : List.length ks <= N.to_nat (List.fold_right N.add 0%N ws)) by lia.
    rewrite sum_N_nat in Hsum'. clear Hsum. rename Hsum' into Hsum.
    assert (Hc : List.In k (List.concat (distribute_by_weight ks ws))).
    { unfold distribute_by_weight. rewrite distribute_aux_concat, length_map.
      pose proof (sum_min_ge (List.length ks) ws) as Hm.
      destruct ws as [|w ws]; simpl in Hsum, Hm.
      - destruct ks; simpl in *; [contradiction | lia].
      - simpl List.repeat. simpl List.concat. simpl List.map. simpl List.list_sum.
        rewrite take_list_app_ge by lia. apply in_or_app; left; auto. }
    apply in_concat in Hc. destruct Hc as [g [Hg Hkg]]. exists g; auto.
Qed.

Lemma in_combine_exists : forall (X Y : Type) (xs : list X) (ys : list Y) y,
    List.length xs = List.length ys -> List.In y ys -> exists x, List.In (x, y) (List.combine xs ys).
Proof.
    intros X Y xs; induction xs as [|x xs IH]; intros ys y Hl Hy; destruct ys as [|y0 ys];
      simpl in *; try discriminate; try contradiction.
    destruct Hy as [<-|Hy].
    - exists x; left; auto.
    - destruct (IH ys y) as [x' Hx']; auto. exists x'; right; auto.
Qed.

Lemma plan_lengths : forall (S : Type) (A : Set) (sinks : list S) ws (ks : list A) p,
    plan sinks ws ks = Some p ->
    List.length sinks = List.length (distribute_by_weight ks ws)
    /\ (N.of_nat (List.length ks) <= List.fold_right N.add 0%N ws)%N
    /\ p = List.combine sinks (distribute_by_weight ks ws).
Proof.
    unfold plan, valid_weights. intros S A sinks ws ks p H.
    destruct (Nat.eqb _ _ && N.leb _ _) eqn:E; [|discriminate].
    apply andb_prop in E. destruct E as [E1 E2].
    apply Nat.eqb_eq in E1. apply N.leb_le in E2. injection H as <-.
    repeat split; auto.
    rewrite <- (length_map (@List.length A)), distribute_group_sizes, length_map; auto.
Qed.

(* every chunk is planned for at least one sink *)
Theorem plan_covers : forall (S : Type) (A : Set) (sinks : list S) ws (ks : list A) p k,
    plan sinks ws ks = Some p -> List.In k ks -> exists s g, List.In (s, g) p /\ List.In k g.
Proof.
    intros S A sinks ws ks p k H Hk.
    destruct (plan_lengths _ _ _ _ _ _ H) as [Hl [Hs ->]].
    destruct (distribute_covers _ ks ws k Hs Hk) as [g [Hg Hkg]].
    destruct (in_combine_exists _ _ sinks _ g Hl Hg) as [s Hsg].
    exists s, g; auto.
Qed.

(* group i holds min w_i n chunks *)
Theorem plan_group_sizes : forall (S : Type) (A : Set) (sinks : list S) ws (ks : list A) p,
    plan sinks ws ks = Some p ->
    List.map (fun sg => List.length (snd sg)) p
    = List.map (fun w => Nat.min (N.to_nat w) (List.length ks)) ws.
Proof.
    intros S A sinks ws ks p H.
    destruct (plan_lengths _ _ _ _ _ _ H) as [Hl [_ ->]].
    rewrite <- distribute_group_sizes.
    revert Hl. generalize (distribute_by_weight ks ws). clear H.
    induction sinks as [|s sinks IH]; intros gs Hl; destruct gs; simpl in *; try discriminate; auto.
    f_equal; auto.
Qed.

(* groups only hold chunks of the input *)
Theorem plan_only_chunks : forall (S : Type) (A : Set) (sinks : list S) ws (ks : list A) p s g k,
    plan sinks ws ks = Some p -> List.In (s, g) p -> List.In k g -> List.In k ks.
Proof.
    intros S A sinks ws ks p s g k H Hsg Hk.
    destruct (plan_lengths _ _ _ _ _ _ H) as [_ [_ ->]].
    apply in_combine_r in Hsg.
    unfold distribute_by_weight in Hsg.
    pose proof (distribute_aux_in _ _ _ _ _ Hsg Hk) as Hr.
    apply in_concat in Hr. destruct Hr as [l [Hl Hkl]].
    apply repeat_spec in Hl. subst; auto.
Qed.

End Plan.

(** * Schedule: try every planned chunk, retry transient failures *)

Definition delay (p : retrypolicy) (a : nat) : N :=
    N.min p.(max_delay_ms) (p.(base_delay_ms) * 2 ^ N.of_nat a).

Definition max_att (p : retrypolicy) : nat := Nat.max 1 p.(max_attempts).

Theorem delay_bounded : forall p a, (delay p a <= p.(max_delay_ms))%N.
Proof. intros; unfold delay; apply N.le_min_l. Qed.

Theorem delay_monotone : forall p a b, a <= b -> (delay p a <= delay p b)%N.
Proof.
    intros p a b H; unfold delay.
    apply N.min_le_compat_l, N.mul_le_mono_l, N.pow_le_mono_r; lia.
Qed.

Section Schedule.
Open Scope list_scope.

Variable Snk : Type.        (* a sink *)
Variable Ky : Set.          (* a chunk key *)
Variable T : Type.          (* the state of the world: the clock, the sinks *)
Variable policy : Snk -> retrypolicy.
Variable copy : T -> Snk -> Ky -> (T * copy_result).
Variable now : T -> (T * N).
Variable sleep_until : T -> N -> (T * N).

Record entry : Type :=
    mkentry { e_sink : Snk; e_key : Ky; e_attempt : nat (* attempts made *);
              e_first : N (* start of the first attempt *); e_due : N; e_last : Z }.

Record final : Type :=
    mkfinal { f_sink : Snk; f_key : Ky; f_attempts : nat; f_result : copy_result }.

(* one attempt: when it started, and when the first attempt on its chunk started *)
Record event : Type :=
    mkevent { v_sink : Snk; v_attempt : nat; v_start : N; v_first : N }.

Definition initial_queue (p : list (Snk * list Ky)) : list entry :=
    List.flat_map (fun sg => List.map (fun k => mkentry (fst sg) k 0 0 0 0) (snd sg)) p.

(* the entry that is due first, and the others *)
Fixpoint pick (q : list entry) : option (entry * list entry) :=
    match q with
    | [] => None
    | e :: r =>
        match pick r with
        | None => Some (e, [])
        | Some (m, r') => if N.ltb m.(e_due) e.(e_due) then Some (m, e :: r') else Some (e, r)
        end
    end.

Definition give_up (e : entry) : final :=
    mkfinal e.(e_sink) e.(e_key) e.(e_attempt) (Failed e.(e_last)).

Inductive decision : Type := Requeue (e : entry) | Final (f : final).

(* what to do after an attempt that started at t and ended at t' with result r *)
Definition decide (e : entry) (first t' : N) (r : copy_result) : decision :=
    let pol := policy e.(e_sink) in
    let a := S e.(e_attempt) in
    match r with
    | Retry c =>
        let due := (t' + delay pol e.(e_attempt))%N in
        if Nat.ltb a (max_att pol) && N.leb due (first + pol.(deadline_ms))%N
        then Requeue (mkentry e.(e_sink) e.(e_key) a first due c)
        else Final (mkfinal e.(e_sink) e.(e_key) a (Failed c))
    | _ => Final (mkfinal e.(e_sink) e.(e_key) a r)
    end.

Fixpoint schedule (fuel : nat) (st : T) (q : list entry) (fs : list final) (log : list event)
    : (T * list final * list event) :=
    match fuel with
    | O => (st, List.rev fs ++ List.map give_up q, List.rev log)
    | S fuel' =>
        match pick q with
        | None => (st, List.rev fs, List.rev log)
        | Some (e, q') =>
            let '(st1, t) := sleep_until st e.(e_due) in
            let first := if Nat.eqb e.(e_attempt) 0 then t else e.(e_first) in
            if N.ltb (first + (policy e.(e_sink)).(deadline_ms))%N t
            then schedule fuel' st1 q' (give_up e :: fs) log
            else
                let '(st2, r) := copy st1 e.(e_sink) e.(e_key) in
                let ev := mkevent e.(e_sink) (S e.(e_attempt)) t first in
                let '(st3, t') := now st2 in
                match decide e first t' r with
                | Requeue e' => schedule fuel' st3 (q' ++ [e']) fs (ev :: log)
                | Final f => schedule fuel' st3 q' (f :: fs) (ev :: log)
                end
        end
    end.

(* enough fuel for every attempt on every entry, and the final empty step *)
Definition fuel_for (q : list entry) : nat :=
    S (List.list_sum (List.map (fun e => max_att (policy e.(e_sink))) q)).

Definition run_plan (st : T) (p : list (Snk * list Ky)) : (T * list final * list event) :=
    let q := initial_queue p in
    schedule (fuel_for q) st q [] [].

Definition distribution_ok (fs : list final) : bool :=
    List.forallb (fun f => is_ok f.(f_result)) fs.

(** ** Properties *)

Lemma pick_none : forall q, pick q = None -> q = [].
Proof. intros [|e r]; simpl; auto. destruct (pick r) as [[m r']|]; [destruct (N.ltb _ _)|]; discriminate. Qed.

Lemma pick_perm : forall q e q', pick q = Some (e, q') -> Permutation q (e :: q').
Proof.
    induction q as [|x r IH]; intros e q' H; simpl in H; [discriminate|].
    destruct (pick r) as [[m r']|] eqn:E.
    - destruct (N.ltb _ _); injection H as <- <-.
      + pose proof (IH m r' eq_refl) as Hr. eapply perm_trans; [apply perm_skip, Hr | apply perm_swap].
      + apply Permutation_refl.
    - injection H as <- <-. apply pick_none in E; subst; auto.
Qed.

(* only a Retry is ever tried again: Copied, Present and Failed end the chunk *)
Theorem no_retry_after_success : forall e first t' r e',
    decide e first t' r = Requeue e' -> exists c, r = Retry c.
Proof. intros e first t' r e'; unfold decide; destruct r; try discriminate; eauto. Qed.

Theorem no_retry_permanent : forall e first t' c e',
    decide e first t' (Failed c) <> Requeue e'.
Proof. discriminate. Qed.

(* the result of the last attempt is kept, a Retry that is not tried again becomes Failed *)
Theorem decide_keeps_result : forall e first t' r f,
    decide e first t' r = Final f ->
    f.(f_result) = match r with Retry c => Failed c | _ => r end.
Proof.
    intros e first t' r f; unfold decide.
    destruct r; try (intro H; injection H as <-; reflexivity).
    destruct (_ && _); [discriminate|]. intro H; injection H as <-; reflexivity.
Qed.

Definition sk_e (e : entry) : Snk * Ky := (e.(e_sink), e.(e_key)).
Definition sk_f (f : final) : Snk * Ky := (f.(f_sink), f.(f_key)).

Lemma decide_requeue : forall e first t' r e',
    decide e first t' r = Requeue e' ->
    sk_e e' = sk_e e /\ e'.(e_attempt) = S e.(e_attempt) /\ e'.(e_attempt) < max_att (policy e.(e_sink)).
Proof.
    intros e first t' r e'; unfold decide; destruct r; try discriminate.
    destruct (Nat.ltb _ _) eqn:E; simpl; [|discriminate].
    destruct (N.leb _ _); simpl; [|discriminate].
    intro H; injection H as <-. apply Nat.ltb_lt in E. unfold sk_e; simpl; auto.
Qed.

Lemma decide_final : forall e first t' r f,
    decide e first t' r = Final f -> sk_f f = sk_e e /\ f.(f_attempts) = S e.(e_attempt).
Proof.
    intros e first t' r f; unfold decide.
    destruct r; try (intro H; injection H as <-; split; reflexivity).
    destruct (_ && _); [discriminate|]. intro H; injection H as <-; split; reflexivity.
Qed.

(* the attempts an entry has left: the fuel it can still use *)
Definition remaining (e : entry) : nat := max_att (policy e.(e_sink)) - e.(e_attempt).

(* a final result: tried at least once and at most max_att times,
   and a success is what copy returned for that sink and chunk *)
Definition good_final (f : final) : Prop :=
    1 <= f.(f_attempts) <= max_att (policy f.(f_sink))
    /\ (is_ok f.(f_result) = true -> exists st, snd (copy st f.(f_sink) f.(f_key)) = f.(f_result)).

Lemma list_sum_perm : forall l l', Permutation l l' -> List.list_sum l = List.list_sum l'.
Proof. intros l l' H; induction H; simpl; lia. Qed.

Lemma remaining_lt_fuel : forall q, List.list_sum (List.map remaining q) < fuel_for q.
Proof.
    intros q; unfold fuel_for. apply Nat.lt_succ_r.
    induction q as [|e q IH]; simpl; auto. unfold remaining at 1. lia.
Qed.

(* the invariants of schedule, as one induction *)
Lemma schedule_inv : forall fuel st q fs log st' fs' log' (K0 : list (Snk * Ky)),
    schedule fuel st q fs log = (st', fs', log') ->
    Permutation (List.map sk_f fs ++ List.map sk_e q) K0 ->
    List.list_sum (List.map remaining q) < fuel ->
    (forall e, List.In e q -> e.(e_attempt) < max_att (policy e.(e_sink))) ->
    (forall f, List.In f fs -> good_final f) ->
    (forall v, List.In v log -> (v.(v_start) <= v.(v_first) + (policy v.(v_sink)).(deadline_ms))%N) ->
    Permutation (List.map sk_f fs') K0
    /\ (forall f, List.In f fs' -> good_final f)
    /\ (forall v, List.In v log' -> (v.(v_start) <= v.(v_first) + (policy v.(v_sink)).(deadline_ms))%N).
Proof.
    induction fuel as [|fuel IH]; intros st q fs log st' fs' log' K0 H Hp Hfuel Hq Hf Hv; simpl in H.
    - (* the fuel never runs out *) exfalso; lia.
    - destruct (pick q) as [[e q']|] eqn:Ep.
      2: { apply pick_none in Ep; subst. injection H as <- <- <-. split; [|split].
           + rewrite map_rev, app_nil_r in *. eapply perm_trans; [apply Permutation_sym, Permutation_rev | exact Hp].
           + intros f Hin; apply Hf, in_rev; auto.
           + intros v Hin; apply Hv, in_rev; auto. }
      pose proof (pick_perm _ _ _ Ep) as Hpe.
      assert (He : e.(e_attempt) < max_att (policy e.(e_sink))) by (apply Hq; eapply Permutation_in; [apply Permutation_sym, Hpe | left; auto]).
      assert (Hq' : forall x, List.In x q' -> x.(e_attempt) < max_att (policy x.(e_sink)))
        by (intros x Hx; apply Hq; eapply Permutation_in; [apply Permutation_sym, Hpe | right; auto]).
      assert (Hp' : Permutation (List.map sk_f fs ++ sk_e e :: List.map sk_e q') K0).
      { eapply perm_trans; [|exact Hp]. apply Permutation_app_head.
        change (sk_e e :: List.map sk_e q') with (List.map sk_e (e :: q')).
        apply Permutation_map, Permutation_sym, Hpe. }
      assert (Hfuel' : remaining e + List.list_sum (List.map remaining q') <= fuel).
      { apply (Permutation_map remaining), list_sum_perm in Hpe. simpl in Hpe. lia. }
      assert (Hre : 1 <= remaining e) by (unfold remaining; lia).
      destruct (sleep_until st e.(e_due)) as [st1 t].
      set (first := if Nat.eqb e.(e_attempt) 0 then t else e.(e_first)) in H.
      destruct (N.ltb _ t) eqn:Edl.
      + eapply IH; [exact H | | lia | exact Hq' | | exact Hv].
        * simpl. eapply perm_trans; [|exact Hp']. apply Permutation_middle.
        * intros f [<-|Hin]; auto. unfold good_final, give_up; simpl. split; [|discriminate].
          (* the first attempt is never past its deadline *)
          subst first. destruct (e_attempt e); simpl in Edl; [apply N.ltb_lt in Edl|]; lia.
      + destruct (copy st1 e.(e_sink) e.(e_key)) as [st2 r] eqn:Ec.
        destruct (now st2) as [st3 t'].
        assert (Hv' : forall v, List.In v (mkevent e.(e_sink) (S e.(e_attempt)) t first :: log) ->
                   (v.(v_start) <= v.(v_first) + (policy v.(v_sink)).(deadline_ms))%N).
        { intros v [<-|Hin]; auto. simpl. apply N.ltb_ge in Edl. exact Edl. }
        destruct (decide e first t' r) as [e'|f] eqn:Ed.
        * destruct (decide_requeue _ _ _ _ _ Ed) as [Hk [Ha Hm]].
          assert (Hr' : remaining e' = remaining e - 1).
          { pose proof Hk as Hk2. unfold sk_e in Hk2. injection Hk2 as Hs _.
            unfold remaining. rewrite Hs, Ha. lia. }
          eapply IH; [exact H | | | | exact Hf | exact Hv'].
          -- rewrite map_app. simpl. rewrite Hk. eapply perm_trans; [|exact Hp'].
             apply Permutation_app_head. apply Permutation_sym, Permutation_cons_append.
          -- rewrite map_app, list_sum_app. simpl. lia.
          -- intros x Hx. apply in_app_or in Hx. destruct Hx as [Hx|[<-|[]]]; auto.
             unfold sk_e in Hk. injection Hk as Hs _. rewrite Hs. exact Hm.
        * destruct (decide_final _ _ _ _ _ Ed) as [Hk Ha].
          eapply IH; [exact H | | lia | exact Hq' | | exact Hv'].
          -- simpl. rewrite Hk. eapply perm_trans; [|exact Hp']. apply Permutation_middle.
          -- intros x [<-|Hx]; auto.
             pose proof (decide_keeps_result _ _ _ _ _ Ed) as Hres.
             unfold sk_f, sk_e in Hk. injection Hk as Hs Hky.
             unfold good_final. rewrite Ha, Hs, Hky. simpl. split; [lia|].
             rewrite Hres. intros Hok. exists st1. rewrite Ec. simpl.
             destruct r; simpl in Hok; try reflexivity; discriminate.
Qed.

Lemma run_plan_inv : forall st p st' fs log,
    run_plan st p = (st', fs, log) ->
    Permutation (List.map sk_f fs) (List.map sk_e (initial_queue p))
    /\ (forall f, List.In f fs -> good_final f)
    /\ (forall v, List.In v log -> (v.(v_start) <= v.(v_first) + (policy v.(v_sink)).(deadline_ms))%N).
Proof.
    intros st p st' fs log H.
    eapply schedule_inv; [exact H | apply Permutation_refl | apply remaining_lt_fuel | | |]; simpl; try contradiction.
    intros e He. unfold initial_queue in He. apply in_flat_map in He.
    destruct He as [sg [_ He]]. apply in_map_iff in He. destruct He as [k [<- _]]. simpl.
    unfold max_att; lia.
Qed.

(* a chunk is tried at most max_attempts times (at least once) *)
Theorem attempts_bounded : forall st p st' fs log f,
    run_plan st p = (st', fs, log) -> List.In f fs -> f.(f_attempts) <= max_att (policy f.(f_sink)).
Proof. intros st p st' fs log f H Hin. apply (run_plan_inv _ _ _ _ _ H) in Hin. destruct Hin as [[_ ?] _]; auto. Qed.

(* every planned chunk is tried: no result is made up without an attempt *)
Theorem tried_at_least_once : forall st p st' fs log f,
    run_plan st p = (st', fs, log) -> List.In f fs -> 1 <= f.(f_attempts).
Proof. intros st p st' fs log f H Hin. apply (run_plan_inv _ _ _ _ _ H) in Hin. destruct Hin as [[? _] _]; auto. Qed.

(* a reported success is what copy returned for that sink and chunk *)
Theorem ok_was_copied : forall st p st' fs log f,
    run_plan st p = (st', fs, log) -> List.In f fs -> is_ok f.(f_result) = true ->
    exists st0, snd (copy st0 f.(f_sink) f.(f_key)) = f.(f_result).
Proof. intros st p st' fs log f H Hin. apply (run_plan_inv _ _ _ _ _ H) in Hin. destruct Hin as [_ ?]; auto. Qed.

(* no attempt starts after the deadline of its chunk *)
Theorem deadline_respected : forall st p st' fs log v,
    run_plan st p = (st', fs, log) -> List.In v log ->
    (v.(v_start) <= v.(v_first) + (policy v.(v_sink)).(deadline_ms))%N.
Proof. intros st p st' fs log v H; apply (run_plan_inv _ _ _ _ _ H). Qed.

(* exactly one final result per planned (sink, chunk) *)
Theorem run_one_result_each : forall st p st' fs log,
    run_plan st p = (st', fs, log) ->
    Permutation (List.map sk_f fs) (List.flat_map (fun sg => List.map (fun k => (fst sg, k)) (snd sg)) p).
Proof.
    intros st p st' fs log H. destruct (run_plan_inv _ _ _ _ _ H) as [Hp _].
    eapply perm_trans; [exact Hp|]. unfold initial_queue.
    rewrite flat_map_concat_map, concat_map, map_map, <- flat_map_concat_map.
    apply Permutation_refl', flat_map_ext. intros sg. rewrite map_map. reflexivity.
Qed.

Theorem run_length : forall st p st' fs log,
    run_plan st p = (st', fs, log) ->
    List.length fs = List.list_sum (List.map (fun sg => List.length (snd sg)) p).
Proof.
    intros st p st' fs log H. apply run_one_result_each in H.
    apply Permutation_length in H. rewrite length_map in H. rewrite H.
    clear H. induction p as [|sg p IH]; simpl; auto.
    rewrite length_app, length_map, IH; reflexivity.
Qed.

(* with a valid plan and a successful run, every chunk reached, or was already on, at least one sink *)
Theorem ok_plan_complete : forall sinks ws (ks : list Ky) p st st' fs log k,
    plan sinks ws ks = Some p -> run_plan st p = (st', fs, log) ->
    distribution_ok fs = true -> List.In k ks ->
    exists f, List.In f fs /\ f.(f_key) = k /\ is_ok f.(f_result) = true.
Proof.
    intros sinks ws ks p st st' fs log k Hp Hr Hok Hk.
    destruct (plan_covers _ _ _ _ _ _ k Hp Hk) as [s [g [Hsg Hkg]]].
    apply run_one_result_each in Hr.
    assert (Hin : List.In (s, k) (List.map sk_f fs)).
    { eapply Permutation_in; [apply Permutation_sym, Hr|]. apply in_flat_map.
      exists (s, g); split; auto. apply in_map_iff; exists k; auto. }
    apply in_map_iff in Hin. destruct Hin as [f [Hf Hin]].
    exists f; split; auto. unfold sk_f in Hf; injection Hf as _ ->. split; auto.
    unfold distribution_ok in Hok. rewrite forallb_forall in Hok. auto.
Qed.

End Schedule.

Arguments f_sink {Snk Ky}.
Arguments f_key {Snk Ky}.
Arguments f_attempts {Snk Ky}.
Arguments f_result {Snk Ky}.
Arguments v_sink {Snk}.
Arguments v_attempt {Snk}.
Arguments v_start {Snk}.
Arguments v_first {Snk}.

(** * Running a distribution on the real sinks *)

Definition sink_policy (s : sink_type) : retrypolicy :=
    match s with S3 x => x.(retry) | FS x => x.(retry) end.

Definition copy_chunk (d : direction) (s : sink_type) (k : string) : copy_result :=
    match d, s with
    | PUT, S3 x => snd (S3Sink.put k x)
    | GET, S3 x => snd (S3Sink.get k x)
    | PUT, FS x => snd (FSSink.put k x)
    | GET, FS x => snd (FSSink.get k x)
    end.

(* shortcut: the sinks have no state yet, so the world is only the clock (unit); thread the sinks through T once they get one *)
Definition run (d : direction) (p : list (sink_type * list string)) : list (final sink_type string) :=
    snd (fst (run_plan sink_type string unit sink_policy
                (fun _ s k => (tt, copy_chunk d s k))
                (fun _ => (tt, Utilities.now_ms tt))
                (fun _ due => (tt, Utilities.sleep_until due))
                tt p)).

(* a reported success is the result of put/get on that sink for that chunk *)
Theorem run_ok_was_copied : forall d p f,
    List.In f (run d p) -> is_ok f.(f_result) = true -> copy_chunk d f.(f_sink) f.(f_key) = f.(f_result).
Proof.
    intros d p f; unfold run.
    destruct (run_plan _ _ _ _ _ _ _ _ _) as [[st' fs] log] eqn:E; simpl.
    intros Hin Hok. destruct (ok_was_copied _ _ _ _ _ _ _ _ _ _ _ _ _ E Hin Hok) as [st0 Hc]. exact Hc.
Qed.

(* a restore needs every chunk from one sink only: the other sinks that hold it may fail *)
Definition restore_ok {Snk : Type} (ks : list string) (fs : list (final Snk string)) : bool :=
    List.forallb (fun k => List.existsb (fun f => String.eqb f.(f_key) k && is_ok f.(f_result)) fs) ks.

Theorem restore_ok_complete : forall Snk ks (fs : list (final Snk string)) k,
    restore_ok ks fs = true -> List.In k ks ->
    exists f, List.In f fs /\ f.(f_key) = k /\ is_ok f.(f_result) = true.
Proof.
    intros Snk ks fs k H Hk. unfold restore_ok in H. rewrite forallb_forall in H.
    apply H, existsb_exists in Hk. destruct Hk as [f [Hf Hb]].
    apply andb_prop in Hb. destruct Hb as [He Hok].
    exists f; repeat split; auto. apply String.eqb_eq; auto.
Qed.

(* decides the exit code: a PUT must reach every planned sink (the copies are wanted) *)
Definition run_ok (d : direction) (ks : list string) (fs : list (final sink_type string)) : bool :=
    match d with
    | PUT => distribution_ok sink_type string fs
    | GET => restore_ok ks fs
    end.

End Distribution.
