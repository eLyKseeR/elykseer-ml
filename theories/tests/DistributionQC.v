(**
      e L y K s e e R
*)

(**
 Property tests of the pure parts of Distribution.v (plan, delay, schedule).

 Each property states a theorem of Distribution.v on random inputs, so a wrongly
 stated theorem shows up before the proof work. The scheduler runs with a fake
 world (clock, scripted copy results), so no axiom is extracted.

 Not in _CoqProject: QuickChick extracts and runs OCaml while coqc runs.
 Run: theories/tests/quickchick.sh (after make)
 *)

From QuickChick Require Import QuickChick.
From Stdlib Require Import ZArith NArith Lists.List Bool.
From LXR Require Import Distribution.
Import ListNotations QcNotation.

Open Scope nat_scope.

(* QuickChick runs 10000 tests per property by default *)

(** * Generators *)

Definition genSmallNat (hi : nat) : G nat := choose (0, hi).

Definition genWeights : G (list nat) :=
    bindGen (choose (0, 4)) (fun n => vectorOf n (genSmallNat 12)).

Definition genKeys : G (list nat) :=
    bindGen (choose (0, 10)) (fun n => returnGen (seq 0 n)).

Definition genResult : G copy_result :=
    freq [ (3, returnGen Copied); (1, returnGen Present);
           (4, returnGen (Retry 503%Z)); (1, returnGen (Failed 403%Z)) ].

Definition genPolicy : G retrypolicy :=
    bindGen (choose (0, 5)) (fun a =>
    bindGen (choose (0, 20)) (fun b =>
    bindGen (choose (0, 100)) (fun m =>
    bindGen (choose (0, 300)) (fun d =>
    returnGen (mkretrypolicy a (N.of_nat b) (N.of_nat m) (N.of_nat d)))))).

Instance showResult : Show copy_result :=
    { show r := (match r with
                 | Copied => "Copied" | Present => "Present"
                 | Retry c => "Retry" | Failed c => "Failed" end)%string }.

Instance showPolicy : Show retrypolicy :=
    { show p := ("max_attempts=" ++ show p.(max_attempts) ++ " base=" ++ show (N.to_nat p.(base_delay_ms))
                 ++ " max=" ++ show (N.to_nat p.(max_delay_ms)) ++ " deadline=" ++ show (N.to_nat p.(deadline_ms)))%string }.

Definition sum (ls : list nat) : nat := fold_right Nat.add 0 ls.
Definition Ns (ws : list nat) : list N := List.map N.of_nat ws.

(** * Plan *)

(* plan_group_sizes *)
Definition prop_group_sizes :=
    forAll genKeys (fun ks => forAll genWeights (fun ws =>
        if List.list_eq_dec Nat.eq_dec (List.map (@length nat) (distribute_by_weight ks (Ns ws)))
                                       (List.map (fun w => Nat.min w (length ks)) ws)
        then true else false)).
QuickChick prop_group_sizes.

(* plan_covers and plan_only_chunks *)
Definition prop_plan_covers :=
    forAll genKeys (fun ks => forAll genWeights (fun ws =>
        let sinks := seq 0 (length ws) in
        match plan sinks (Ns ws) ks with
        | None => checker true
        | Some p =>
            checker (forallb (fun k => existsb (fun sg => existsb (Nat.eqb k) (snd sg)) p) ks
                     && forallb (fun sg => forallb (fun k => existsb (Nat.eqb k) ks) (snd sg)) p)
        end)).
QuickChick prop_plan_covers.

(* a plan exists exactly for the weights the CLI accepts: one per sink, summing up to the chunks *)
Definition prop_valid_weights :=
    forAll genKeys (fun ks => forAll genWeights (fun ws => forAll (choose (0, 4)) (fun nsinks =>
        Bool.eqb (match plan (seq 0 nsinks) (Ns ws) ks with Some _ => true | None => false end)
                 ((length ws =? nsinks) && (length ks <=? sum ws))))).
QuickChick prop_valid_weights.

(** * Delay *)

Definition prop_delay :=
    forAll genPolicy (fun p => forAll (genSmallNat 30) (fun a =>
        (delay p a <=? max_delay_ms p)%N && (delay p a <=? delay p (S a))%N)).
QuickChick prop_delay.

(** * Schedule, in a fake world *)

(* the world: the clock in ms, and how often each (sink, key) was tried *)
Definition W := (N * list (nat * nat * nat))%type.

Fixpoint count (sk : nat * nat) (cs : list (nat * nat * nat)) : nat :=
    match cs with
    | [] => 0
    | (s, k, n) :: r => if (fst sk =? s) && (snd sk =? k) then n else count sk r
    end.

(* the result of attempt a (1-based) on (sink, key) *)
Definition scripted (script : list (list copy_result)) (sk : nat * nat) (a : nat) : copy_result :=
    nth (a - 1) (nth (fst sk * 16 + snd sk) script []) Copied.

(* every copy takes 1 ms *)
Definition fake_copy (script : list (list copy_result)) (w : W) (s : nat) (k : nat) : (W * copy_result) :=
    let a := S (count (s, k) (snd w)) in
    ((fst w + 1, (s, k, a) :: snd w)%N, scripted script (s, k) a).

Definition fake_now (w : W) : (W * N) := (w, fst w).
Definition fake_sleep (w : W) (due : N) : (W * N) := let t := N.max (fst w) due in ((t, snd w), t).

Definition fake_run (pol : retrypolicy) script p :=
    run_plan nat nat W (fun _ => pol) (fake_copy script) fake_now fake_sleep (0%N, []) p.

Definition genScript : G (list (list copy_result)) := vectorOf 64 (vectorOf 6 genResult).

Definition genPlan : G (list (nat * list nat)) :=
    bindGen genKeys (fun ks => bindGen genWeights (fun ws =>
        returnGen (match plan (seq 0 (length ws)) (Ns ws) ks with Some p => p | None => [] end))).

Definition with_run (f : retrypolicy -> list (list copy_result) -> list (nat * list nat) ->
                         list (final nat nat) -> list (event nat) -> bool) :=
    forAll genPolicy (fun pol => forAll genScript (fun script => forAll genPlan (fun p =>
        let '(_, fs, log) := fake_run pol script p in f pol script p fs log))).

(* run_length *)
Definition prop_run_length :=
    with_run (fun _ _ p fs _ => length fs =? sum (List.map (fun sg => length (snd sg)) p)).
QuickChick prop_run_length.

(* attempts_bounded *)
Definition prop_attempts_bounded :=
    with_run (fun pol _ _ fs _ => forallb (fun f => f.(f_attempts) <=? Nat.max 1 pol.(max_attempts)) fs).
QuickChick prop_attempts_bounded.

(* tried_at_least_once *)
Definition prop_tried_once :=
    with_run (fun _ _ _ fs _ => forallb (fun f => 1 <=? f.(f_attempts)) fs).
QuickChick prop_tried_once.

(* deadline_respected *)
Definition prop_deadline_respected :=
    with_run (fun pol _ _ _ log => forallb (fun v => (v.(v_start) <=? v.(v_first) + pol.(deadline_ms))%N) log).
QuickChick prop_deadline_respected.

(* no_retry_after_success, no_retry_permanent: every attempt before the last one was a Retry,
   and the last result is kept (a Retry that is given up on becomes Failed) *)
Definition is_retry r := match r with Retry _ => true | _ => false end.
Definition prop_no_retry :=
    with_run (fun _ script _ fs _ =>
        forallb (fun f =>
            let sk := (f.(f_sink), f.(f_key)) in
            forallb (fun a => is_retry (scripted script sk a)) (seq 1 (f.(f_attempts) - 1))
            && match f.(f_result) with
               | Copied | Present => (0 <? f.(f_attempts)) && negb (is_retry (scripted script sk f.(f_attempts)))
                                     && match scripted script sk f.(f_attempts), f.(f_result) with
                                        | Copied, Copied | Present, Present => true | _, _ => false end
               | Failed _ => true
               | Retry _ => false
               end) fs).
QuickChick prop_no_retry.

(* ok_plan_complete *)
Definition prop_ok_complete :=
    forAll genPolicy (fun pol => forAll genScript (fun script =>
    forAll genKeys (fun ks => forAll genWeights (fun ws =>
        match plan (seq 0 (length ws)) (Ns ws) ks with
        | None => true
        | Some p =>
            let '(_, fs, _) := fake_run pol script p in
            negb (distribution_ok nat nat fs)
            || forallb (fun k => existsb (fun f => (f.(f_key) =? k) && is_ok f.(f_result)) fs) ks
        end)))).
QuickChick prop_ok_complete.
