(**
      e L y K s e e R
*)

Module Export AssemblyCache.

Require Import Program.
Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String.

Open Scope N_scope.
Open Scope list_scope.
Open Scope string_scope.

From LXR Require Import Assembly.
From LXR Require Import Buffer.
From LXR Require Import Configuration.
From LXR Require Import Conversion.
From LXR Require Import Environment.
From LXR Require Import Nchunks.
From LXR Require Import Utilities.

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
        ; qbuffer : BufferPlain.buffer_t
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
        { acenvs : list environment
        ; acsize : nat
        ; acwriteenv : environment
        ; acconfig : configuration
        ; acwriteq : writequeue
        ; acreadq : readqueue
        }.


Definition prepare_assemblycache (c : configuration) (size : positive) : assemblycache :=
    (* let celist := Utilities.make_list size in *)
    {| acenvs := nil (* List.map (fun _i => Environment.initial_environment c) celist *)
     ; acsize := Pos.to_nat size
     ; acwriteenv := Environment.initial_environment c
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

(* extract the last element of a list if it exists *)
Fixpoint last_opt { a : Type } (l : list a) : option a :=
    match l with
    | nil => None
    | a :: nil => Some a
    | _a :: l => last_opt l
    end.

(* ensure that an environment with assembly (by aid) is available 
   and that it is in the head position of the list of envs *)
Program Definition ensure_assembly (ac0 : assemblycache) (sel_aid : Assembly.aid_t) : option (environment * assemblycache) :=
    match ac0.(acenvs) with
    | nil => (* create first env *)
        match restore_assembly (Environment.initial_environment ac0.(acconfig)) sel_aid with
        | None => None
        | Some env =>
           Some (env, {| acenvs := env :: nil; acsize := ac0.(acsize); acwriteenv := ac0.(acwriteenv); acconfig := ac0.(acconfig);
                         acreadq := ac0.(acreadq); acwriteq := ac0.(acwriteq) |})
        end
    | e1 :: r => if String.eqb (aid e1.(cur_assembly)) sel_aid then Some (e1, ac0)
        else
            let found := List.filter (fun e => String.eqb (aid e.(cur_assembly)) sel_aid) r in
            match found with
            | efound :: _ =>
                Some (efound, {| acenvs := efound :: e1 :: List.filter (fun e => negb (String.eqb (aid e.(cur_assembly)) sel_aid)) r;
                                 acsize := ac0.(acsize); acwriteenv := ac0.(acwriteenv); acconfig := ac0.(acconfig);
                                 acreadq := ac0.(acreadq); acwriteq := ac0.(acwriteq) |})
            | nil =>
                match last_opt r with
                | None => match restore_assembly (Environment.initial_environment ac0.(acconfig)) sel_aid with
                          | None => None
                          | Some env =>
                              Some (env, {| acenvs := env :: ac0.(acenvs); acsize := ac0.(acsize); acwriteenv := ac0.(acwriteenv); acconfig := ac0.(acconfig);
                                            acreadq := ac0.(acreadq); acwriteq := ac0.(acwriteq) |})
                          end
                | Some env0 => match restore_assembly env0 sel_aid with
                               | None => None
                               | Some env =>
                                   Some (env, {| acenvs := env :: List.filter (fun e => negb (String.eqb (aid e.(cur_assembly)) (aid env0.(cur_assembly)))) ac0.(acenvs);
                                                 acsize := ac0.(acsize); acwriteenv := ac0.(acwriteenv); acconfig := ac0.(acconfig);
                                                 acreadq := ac0.(acreadq); acwriteq := ac0.(acwriteq) |})
                               end
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
            let _ := assembly_get_content (id_assembly_full_buffer_from_writable env.(cur_buffer)) h.(qrlen) h.(qapos) buf in
            run_read_requests ac1 r ({| readrequest := h; rresult := buf |} :: res)
        end
    end.

Program Fixpoint run_write_requests (ac0 : assemblycache) (reqs : list writequeueentity) (res : list writequeueresult) : (list writequeueresult * assemblycache) :=
    match reqs with
    | nil => (res, ac0)
    | h :: r =>
        let env := Environment.backup ac0.(acwriteenv) h.(qfhash) h.(qfpos) h.(qbuffer) in
        let ac1 := {| acenvs := ac0.(acenvs); acsize := ac0.(acsize); acwriteenv := env; acconfig := ac0.(acconfig);
                      acreadq := ac0.(acreadq); acwriteq := {| wqueue := nil; wqueuesz := wqueuesz ac0.(acwriteq) |} |} in
        run_write_requests ac1 r ({| writerequest := h; wresult := {| qaid := aid env.(cur_assembly); qapos := apos env.(cur_assembly); qrlen := buffer_len h.(qbuffer) |} |} :: res)
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
                      acreadq := {| rqueue := List.filter (fun e => negb (String.eqb e.(qaid) aid)) r; rqueuesz := rqueuesz ac0.(acreadq) |};
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

(* finalise and extract from writable environment *)
Program Definition close (ac0 : assemblycache) : assemblycache :=
    let env := finalise_assembly ac0.(acwriteenv) in
    {| acenvs := nil; acsize := ac0.(acsize); acwriteenv := env; acconfig := ac0.(acconfig);
       acreadq := ac0.(acreadq); acwriteq := ac0.(acwriteq) |}.

End AssemblyCache.