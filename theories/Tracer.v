(**
      e L y K s e e R
*)

Require Import NArith PArith.
From Coq Require Import Strings.String.

Module Export Tracer.

Inductive loglevel : Set :=
    | debug
    | info
    | warning
    | error .
Print loglevel.

Record tracer :=
    mktracer
        { logDebug : string -> option unit
        ; logInfo : string -> option unit
        ; logWarning : string -> option unit
        ; logError : string -> option unit
        }.

Definition ignore {A} (x : A) : option unit := None.

Definition nullTracer : tracer :=
    mktracer ignore ignore ignore ignore.

Axiom output_stdout : loglevel -> string -> option unit.
Definition stdoutTracerDebug : tracer :=
    mktracer (output_stdout debug) (output_stdout info) (output_stdout warning) (output_stdout error).

Definition stdoutTracerInfo : tracer :=
    mktracer ignore (output_stdout info) (output_stdout warning) (output_stdout error).

Definition stdoutTracerWarning : tracer :=
    mktracer ignore ignore (output_stdout warning) (output_stdout error).

Definition stdoutTracerError : tracer :=
    mktracer ignore ignore ignore (output_stdout error).

Definition log (t : tracer) (ll : loglevel) (m : string) : option unit :=
    match ll with
    | debug => t.(logDebug) m
    | info => t.(logInfo) m
    | warning => t.(logWarning) m
    | error => t.(logError) m
    end.

Definition conditionalTrace {A} (t : tracer) (condition : bool) (ll_true : loglevel) (true_msg : option string) (true_computation : unit -> option A) (ll_false : loglevel) (false_msg : option string) (false_computation : unit -> option A) : option A :=
    if (condition)
    then
        match true_msg with
        | Some m => match log t ll_true m with
                    | Some tt => 
                        true_computation tt
                    | None => None
                    end
        | None => true_computation tt
        end
    else
        match false_msg with
        | Some m => match log t ll_false m with
                    | Some tt => 
                        false_computation tt
                    | None => None
                    end
        | None => false_computation tt
        end.

Definition optionalTrace {A} {B} (t : tracer) (computation : option B) (ll_none : loglevel) (none_msg : option string) (computation_none : unit -> option A) (ll_some : loglevel) (some_msg : option string) (computation_some : B -> option A) : option A :=
    match (computation) with
    | None =>
        match none_msg with
        | Some m => match log t ll_none m with
                    | Some tt => 
                        computation_none tt
                    | None => None
                    end
        | None => computation_none tt
        end
    | Some b =>
        match some_msg with
        | Some m => match log t ll_some m with
                    | Some tt => 
                        computation_some b
                    | None => None
                    end
        | None => computation_some b
        end
    end.

End Tracer.