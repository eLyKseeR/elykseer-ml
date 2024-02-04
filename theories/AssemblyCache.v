(**
      e L y K s e e R
*)

Require Import Program.
Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String Lia.

From LXR Require Import Assembly.
From LXR Require Import Configuration.
From LXR Require Import Conversion.
From LXR Require Import Cstdio.
From LXR Require Import Environment.
From LXR Require Import Nchunks.
From LXR Require Import Utilities.

Module Export AssemblyCache.

Open Scope N_scope.
Open Scope list_scope.
Open Scope string_scope.

Record readqueueentity : Type :=
    mkreadqueueentity
        { qaid : Assembly.aid_t
        ; qapos : N
        ; qrlen : N
        }.
Record readqueueresult : Type :=
    mkreadqueueresult
        { readrequest : readqueueentity
        ; rresult : BufferPlain.buffer_t
        }.

Record writequeueentity : Type :=
    mkwritequeueentity
        { qfhash : string
        ; qfpos : N
        ; qbuffer : BufferPlain.buffer_t (* implicit write data size = buffer size *)
        }.
Record writequeueresult : Type :=
    mkwritequeueresult
        { writerequest : writequeueentity
        ; wresult : readqueueentity
        }.

Definition qsize : positive := 12.

Record readqueue : Type :=
    mkreadqueue
        { rqueue : list readqueueentity
        ; rqueuesz : positive
        }.

Record writequeue : Type :=
    mkwritequeue
        { wqueue : list writequeueentity
        ; wqueuesz : positive
        }.

Record assemblycache : Type :=
    mkassemblycache
        { acenvs : list EnvironmentReadable.E
        ; acsize : nat
            (* ^ read environments of a fixed maximal size *)
        ; acwriteenv : EnvironmentWritable.E
            (* ^ write environment, exactly one *)
        ; acconfig : configuration
            (* ^ configuration needed to create new assemblies *)
        ; acwriteq : writequeue
            (* ^ writer queue *)
        ; acreadq : readqueue
            (* ^ reader queue *)
        }.


Definition prepare_assemblycache (c : configuration) (size : positive) : assemblycache :=
    {| acenvs := nil
     ; acsize := Pos.to_nat size
     ; acwriteenv := EnvironmentWritable.initial_environment c
     ; acconfig := c
     ; acwriteq := (mkwritequeue nil qsize)
     ; acreadq := (mkreadqueue nil qsize)
    |}.

Program Definition enqueue_write_request (ac : assemblycache) (req : writequeueentity) : (bool * assemblycache) :=
    let ln := List.length (wqueue ac.(acwriteq)) in
    if N.leb (pos2N qsize) (nat2N ln) then
        (false, ac)
    else
        (true, {| acenvs := ac.(acenvs); acsize := ac.(acsize); acwriteenv := ac.(acwriteenv); acconfig := ac.(acconfig);
                  acwriteq := {| wqueue := List.app (wqueue ac.(acwriteq)) (req :: nil); wqueuesz := wqueuesz ac.(acwriteq) |};
                  acreadq := ac.(acreadq)  |}).

Program Definition enqueue_read_request (ac : assemblycache) (req : readqueueentity) : (bool * assemblycache) :=
    let ln := List.length (rqueue ac.(acreadq)) in
    if N.leb (pos2N qsize) (nat2N ln) then
        (false, ac)
    else
        (true, {| acenvs := ac.(acenvs); acsize := ac.(acsize); acwriteenv := ac.(acwriteenv); acconfig := ac.(acconfig);
                  acreadq := {| rqueue := List.app (rqueue ac.(acreadq)) (req :: nil); rqueuesz := rqueuesz ac.(acreadq) |};
                  acwriteq := ac.(acwriteq)  |}).

Program Definition try_restore_assembly (config : Configuration.configuration) (sel_aid : Assembly.aid_t) : option EnvironmentReadable.E :=
    EnvironmentReadable.restore_assembly (EnvironmentReadable.initial_environment config) sel_aid.

Program Definition set_envs (ac0 : assemblycache) (envs : list EnvironmentReadable.E) : assemblycache :=
    {| acenvs := envs; acsize := ac0.(acsize);
       acwriteenv := ac0.(acwriteenv); acconfig := ac0.(acconfig);
       acreadq := ac0.(acreadq); acwriteq := ac0.(acwriteq) |}.

(* ensure that an environment with assembly (by aid) is available
   and that it is in the head position of the list of envs *)
Program Definition ensure_assembly (ac0 : assemblycache) (sel_aid : Assembly.aid_t) : option (EnvironmentReadable.E * assemblycache) :=
    match ac0.(acenvs) with
    | nil => (* create first env *)
        match try_restore_assembly ac0.(acconfig) sel_aid with
        | None => None
        | Some env =>
           Some (env, set_envs ac0 (env :: nil))
        end
    | e1 :: nil =>
        if String.eqb (aid e1.(cur_assembly AssemblyPlainFull.B)) sel_aid then
            Some (e1, ac0)
        else
            match try_restore_assembly ac0.(acconfig) sel_aid with
            | None => None
            | Some env =>
                if Nat.eqb ac0.(acsize) 1
                then Some (env, set_envs ac0 (env :: nil))
                else Some (env, set_envs ac0 (env :: e1 :: nil))
            end
    | e1 :: r =>
        if String.eqb (aid e1.(cur_assembly AssemblyPlainFull.B)) sel_aid then
            (* found in first position *)
            Some (e1, ac0)
        else
            let found := List.filter (fun e => String.eqb (aid e.(cur_assembly AssemblyPlainFull.B)) sel_aid) r in
            match found with
            | efound :: _ =>
                (* found further down -> move to first position *)
                let r' := List.filter (fun e => negb (String.eqb (aid e.(cur_assembly AssemblyPlainFull.B)) sel_aid)) r in
                Some (efound, set_envs ac0 (efound :: e1 :: r'))
            | nil =>
                match try_restore_assembly ac0.(acconfig) sel_aid with
                | None => None
                | Some env =>
                    (* check number of envs <= acsize *)
                    let lr := if Nat.leb ac0.(acsize) (List.length ac0.(acenvs))
                                then List.removelast ac0.(acenvs)
                                else ac0.(acenvs) in
                    Some (env, set_envs ac0 (env :: lr))
                end
            end
    end.

Program Fixpoint run_read_requests (ac0 : assemblycache) (reqs : list readqueueentity) (res : list readqueueresult) : (list readqueueresult * assemblycache) :=
    match reqs with
    | nil => (res, ac0)
    | h :: r =>
        let aid := h.(qaid) in
        match ensure_assembly ac0 aid with
        | None => (res, ac0)
        | Some (env, ac1) =>
            let buf := BufferPlain.buffer_create h.(qrlen) in
            let _ := assembly_get_content env.(cur_buffer AssemblyPlainFull.B) h.(qrlen) h.(qapos) buf in
            run_read_requests ac1 r ({| readrequest := h; rresult := buf |} :: res)
        end
    end.

Program Fixpoint run_write_requests (ac0 : assemblycache) (reqs : list writequeueentity) (res : list writequeueresult)
                                  : (list writequeueresult * assemblycache) :=
    match reqs with
    | nil => (res, ac0)
    | h :: r =>
        let (apos, env) := EnvironmentWritable.backup ac0.(acwriteenv) h.(qfhash) h.(qfpos) h.(qbuffer) in
        let ac1 := {| acenvs := ac0.(acenvs); acsize := ac0.(acsize); acwriteenv := env; acconfig := ac0.(acconfig);
                      acreadq := ac0.(acreadq); acwriteq := {| wqueue := nil; wqueuesz := wqueuesz ac0.(acwriteq) |} |} in
        run_write_requests ac1 r ({| writerequest := h;
                                     wresult := {| qaid := env.(cur_assembly AssemblyPlainWritable.B).(aid);
                                                   qapos := apos;
                                                   qrlen := buffer_len h.(qbuffer) |} |} :: res)
    end.


(* iterate through the read queue and fulfill each request.
   eliminates the selected requests (by aid) from the queue.
   returns a list of results. *)
Program Definition iterate_read_queue (ac0 : assemblycache) : (list readqueueresult * assemblycache) :=
    match rqueue ac0.(acreadq) with
    | nil => (nil, ac0)
    | h :: r =>
        let aid := h.(qaid) in
        let sel := List.filter (fun e => String.eqb e.(qaid) aid) r in
        let ac1 := {| acenvs := ac0.(acenvs); acsize := ac0.(acsize); acwriteenv := ac0.(acwriteenv); acconfig := ac0.(acconfig);
                      acreadq := {| rqueue := List.filter (fun e => negb (String.eqb e.(qaid) aid)) r; rqueuesz := ac0.(acreadq).(rqueuesz) |};
                      acwriteq := ac0.(acwriteq)|} in
        run_read_requests ac1 (h :: sel) nil
    end.

(* iterate through the write queue and fulfill each request.
   eliminates the selected requests (by aid) from the queue.
   returns a list of results. *)
Program Definition iterate_write_queue (ac0 : assemblycache) : (list writequeueresult * assemblycache) :=
    match wqueue ac0.(acwriteq) with
    | nil => (nil, ac0)
    | h :: r =>
        let ac1 := {| acenvs := ac0.(acenvs); acsize := ac0.(acsize); acwriteenv := ac0.(acwriteenv); acconfig := ac0.(acconfig);
                      acreadq := ac0.(acreadq); acwriteq := {| wqueue := nil; wqueuesz := wqueuesz ac0.(acwriteq) |} |} in
        run_write_requests ac1 (h :: r) nil
    end.

(* finalise and recreate writable environment, and extract chunks from it *)
Program Definition flush (ac0 : assemblycache) : assemblycache :=
    let env := EnvironmentWritable.finalise_and_recreate_assembly ac0.(acwriteenv) in
    {| acenvs := nil; acsize := ac0.(acsize); acwriteenv := env; acconfig := ac0.(acconfig);
       acreadq := ac0.(acreadq); acwriteq := ac0.(acwriteq) |}.

(* terminate cache: finalise writable environment and extract chunks from it *)
Program Definition close (ac0 : assemblycache) : assemblycache :=
    let env := EnvironmentWritable.finalise_assembly ac0.(acwriteenv) in
    {| acenvs := nil; acsize := ac0.(acsize); acwriteenv := env; acconfig := ac0.(acconfig);
       acreadq := ac0.(acreadq); acwriteq := ac0.(acwriteq) |}.



Section Validation.

(* our database: the assembly with id "aid_found" exists and can be restored *)
Axiom db_env_for_known_aid : forall e c,
    let env :=
        {| fblocks := e.(fblocks AssemblyPlainFull.B); keys := e.(keys AssemblyPlainFull.B); config := e.(config AssemblyPlainFull.B); cur_buffer := e.(cur_buffer AssemblyPlainFull.B);
        cur_assembly := mkassembly c.(config_nchunks) "aid_found" 0 |} in
    try_restore_assembly c "aid_found" = Some env.

(* all other assemblies are not found. *)
Axiom db_env_none_default : forall a c,
    let env := EnvironmentReadable.initial_environment c in
    try_restore_assembly c a = None.


Lemma emptyread_id : forall c k, let ac := prepare_assemblycache c k in iterate_read_queue ac = (nil, ac).
Proof.
    intros.
    unfold iterate_read_queue. simpl.
    reflexivity.
Qed.

Lemma emptywrite_id : forall c k, let ac := prepare_assemblycache c k in iterate_write_queue ac = (nil, ac).
Proof.
    intros.
    unfold iterate_write_queue. simpl.
    reflexivity.
Qed.

Section Unfinished.
Axiom run_read_requests_same_length : forall ac reqs,
    let len1 := List.length reqs in
    let (lres, _) := run_read_requests ac reqs nil in
    let len2 := List.length lres in
    len1 = len2.
(* Proof.
    induction reqs. intros.
    - simpl. reflexivity.
    - admit.
Admitted.  *)

(* running the read queue of length _n_ returns _n_ results *)
Theorem read_returns_same_length : forall ac reqs,
    let len1 := List.length reqs in
    let (lres, ac') := (run_read_requests ac reqs nil) in
    let len2 := List.length lres in
    len1 = len2.
Proof.
    intros.
    induction reqs. simpl.
    - reflexivity.
    - apply run_read_requests_same_length. 
Qed.

End Unfinished.

(* running the read queue with a single read request,
   returns exactly one result *)
Example single_readrequest_returns_one_result : forall c,
    let ac := prepare_assemblycache c 3 in
    let readreq := (mkreadqueueentity "aid_found" 123 457) in
    let (true, ac') := enqueue_read_request ac readreq in
    let (readres, ac'') := iterate_read_queue ac' in
    List.length readres = S 0.
Proof.
    intros Config ac.
    unfold enqueue_read_request. simpl.
    unfold iterate_read_queue.
    unfold filter. simpl.
    unfold ensure_assembly. simpl.
    set (e0 := EnvironmentReadable.initial_environment Config).
    rewrite db_env_for_known_aid with (e := e0).
    auto.
Qed.

Example first_env_for_empty_cache : forall c,
    let ac0 := prepare_assemblycache c 3 in
    let e0 := EnvironmentReadable.initial_environment c in
    let env1 := match try_restore_assembly c "aid_found" with
               | Some e => e
               | None => e0
               end in
    let ac' := {| acenvs := env1 :: nil; acsize := ac0.(acsize); acwriteenv := ac0.(acwriteenv); acconfig := ac0.(acconfig);
                  acreadq := ac0.(acreadq); acwriteq := ac0.(acwriteq) |} in
    ensure_assembly ac0 "aid_found" = Some (env1, ac').
Proof.
    intro Config.
    unfold ensure_assembly. simpl.
    set (e0 := EnvironmentReadable.initial_environment Config).
    rewrite db_env_for_known_aid with (e := e0).
    unfold set_envs. reflexivity.
Qed.

(* Axiom aid_of_initial_environment : forall c,
    aid (cur_assembly (initial_environment c)) = "". *)

Example second_of3_env_for_not_filled_cache : forall c,
    let AB := AssemblyPlainFull.B in
    let ac0 := prepare_assemblycache c 3 in
    let e0 := EnvironmentReadable.initial_environment c in
    let env1 := let e := EnvironmentReadable.initial_environment c in
        {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
        cur_assembly := mkassembly c.(config_nchunks) "aid_some" 0 |} in
    (* from this assembly cache: *)
    let ac1 := set_envs ac0 (env1 :: nil) in
    let env2 := match try_restore_assembly c "aid_found" with
                | Some e =>
                    {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
                    cur_assembly := mkassembly c.(config_nchunks) "aid_found" 0 |}
                | None => EnvironmentReadable.initial_environment c
                end in
    (* to this assembly cache: env2 added to head *)
    let ac' := {| acenvs := env2 :: env1 :: nil; acsize := ac1.(acsize); acwriteenv := ac1.(acwriteenv); acconfig := ac1.(acconfig);
                  acreadq := ac1.(acreadq); acwriteq := ac1.(acwriteq) |} in
    ensure_assembly ac1 "aid_found" = Some (env2, ac').
Proof.
    intro Config.
    unfold ensure_assembly. simpl.
    set (e0 := EnvironmentReadable.initial_environment Config).
    rewrite db_env_for_known_aid with (e := e0).
    unfold set_envs. simpl. reflexivity.
Qed.

Example second_of2_env_for_not_filled_cache : forall c,
    let AB := AssemblyPlainFull.B in
    let ac0 := prepare_assemblycache c 2 in
    let e0 := EnvironmentReadable.initial_environment c in
    let env1 := let e := EnvironmentReadable.initial_environment c in
        {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
        cur_assembly := mkassembly c.(config_nchunks) "aid_some" 0 |} in
    (* from this assembly cache: *)
    let ac1 := set_envs ac0 (env1 :: nil) in
    let env2 := match try_restore_assembly c "aid_found" with
                | Some e =>
                    {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
                    cur_assembly := mkassembly c.(config_nchunks) "aid_found" 0 |}
                | None => EnvironmentReadable.initial_environment c
                end in
    (* to this assembly cache: env2 added to head *)
    let ac' := {| acenvs := env2 :: env1 :: nil; acsize := ac1.(acsize); acwriteenv := ac1.(acwriteenv); acconfig := ac1.(acconfig);
                  acreadq := ac1.(acreadq); acwriteq := ac1.(acwriteq) |} in
    ensure_assembly ac1 "aid_found" = Some (env2, ac').
Proof.
    intro Config.
    unfold ensure_assembly. simpl.
    set (e0 := EnvironmentReadable.initial_environment Config).
    rewrite db_env_for_known_aid with (e := e0).
    unfold set_envs. simpl. reflexivity.
Qed.

Example third_of2_env_for_filled_cache : forall c,
    let AB := AssemblyPlainFull.B in
    let ac0 := prepare_assemblycache c 2 in
    let e0 := EnvironmentReadable.initial_environment c in
    let env1 := let e := EnvironmentReadable.initial_environment c in
        {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
        cur_assembly := mkassembly c.(config_nchunks) "aid_some" 0 |} in
    let env2 := let e := EnvironmentReadable.initial_environment c in
        {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
        cur_assembly := mkassembly c.(config_nchunks) "aid_other" 0 |} in
    (* from this "full" assembly cache: env3 at head, env1 dropped *)
    let ac1 := set_envs ac0 (env2 :: env1 :: nil) in
    let env3 := match try_restore_assembly c "aid_found" with
                | Some e =>
                    {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
                    cur_assembly := mkassembly c.(config_nchunks) "aid_found" 0 |}
                | None => EnvironmentReadable.initial_environment c
                end in
    (* to this "full" assembly cache: *)
    let ac' := {| acenvs := env3 :: env2 :: nil; acsize := ac1.(acsize); acwriteenv := ac1.(acwriteenv); acconfig := ac1.(acconfig);
                  acreadq := ac1.(acreadq); acwriteq := ac1.(acwriteq) |} in
    ensure_assembly ac1 "aid_found" = Some (env3, ac').
Proof.
    intro Config.
    unfold ensure_assembly. simpl.
    set (e0 := EnvironmentReadable.initial_environment Config).
    rewrite db_env_for_known_aid with (e := e0).
    unfold set_envs. simpl. reflexivity.
Qed.

Example found_assembly_in_filled_cache_of_size_2 : forall c,
    let AB := AssemblyPlainFull.B in
    let ac0 := prepare_assemblycache c 2 in
    let e0 := EnvironmentReadable.initial_environment c in
    let env1 := let e := EnvironmentReadable.initial_environment c in
        {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
        cur_assembly := mkassembly c.(config_nchunks) "aid_found" 0 |} in
    let env2 := let e := EnvironmentReadable.initial_environment c in
        {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
        cur_assembly := mkassembly c.(config_nchunks) "aid_other" 0 |} in
    (* from this "full" assembly cache: *)
    let ac1 := set_envs ac0 (env2 :: env1 :: nil) in
    (* to this "full" assembly cache: env1 moves to head *)
    let ac' := {| acenvs := env1 :: env2 :: nil; acsize := ac1.(acsize); acwriteenv := ac1.(acwriteenv); acconfig := ac1.(acconfig);
                  acreadq := ac1.(acreadq); acwriteq := ac1.(acwriteq) |} in
    ensure_assembly ac1 "aid_found" = Some (env1, ac').
Proof.
    intro Config.
    unfold ensure_assembly. simpl.
    set (e0 := EnvironmentReadable.initial_environment Config).
    unfold set_envs. simpl. reflexivity.
Qed.

Example found_assembly_in_filled_cache_of_size_3 : forall c,
    let AB := AssemblyPlainFull.B in
    let ac0 := prepare_assemblycache c 3 in
    let e0 := EnvironmentReadable.initial_environment c in
    let env1 := let e := EnvironmentReadable.initial_environment c in
        {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
        cur_assembly := mkassembly c.(config_nchunks) "aid_found" 0 |} in
    let env2 := let e := EnvironmentReadable.initial_environment c in
        {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
        cur_assembly := mkassembly c.(config_nchunks) "aid_other" 0 |} in
    let env3 := let e := EnvironmentReadable.initial_environment c in
        {| fblocks := e.(fblocks AB); keys := e.(keys AB); config := e.(config AB); cur_buffer := e.(cur_buffer AB);
        cur_assembly := mkassembly c.(config_nchunks) "aid_latest" 0 |} in
    (* from this "full" assembly cache: *)
    let ac1 := set_envs ac0 (env3 :: env2 :: env1 :: nil) in
    (* to this "full" assembly cache: env1 moves to head *)
    let ac' := {| acenvs := env1 :: env3 :: env2 :: nil; acsize := ac1.(acsize); acwriteenv := ac1.(acwriteenv); acconfig := ac1.(acconfig);
                  acreadq := ac1.(acreadq); acwriteq := ac1.(acwriteq) |} in
    ensure_assembly ac1 "aid_found" = Some (env1, ac').
Proof.
    intro Config.
    unfold ensure_assembly. simpl.
    set (e0 := EnvironmentReadable.initial_environment Config).
    unfold set_envs. simpl. reflexivity.
Qed.

End Validation.


End AssemblyCache.