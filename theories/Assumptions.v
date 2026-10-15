(**
      e L y K s e e R
*)

(**
 Module: Assumptions
 Description: reports the trust base of the specification.

 For every proved statement and for the entry points that get extracted to
 OCaml, `Print Assumptions` lists the axioms and parameters they depend on
 (FFI shims, unproved specifications, Admitted lemmas).

 The output is compared against the committed allowlist
 `theories/Assumptions.expected` by `theories/assumptions.sh check`, which also
 runs in CI. A new axiom therefore shows up as a diff in review.

 To accept a change on purpose: `theories/assumptions.sh update` and commit the result.
 *)

From LXR Require Import Assembly AssemblyCache CheckApos Environment Nchunks Processor.

(** * Proved statements

    These should ideally be closed under the global context. *)

Locate CheckApos.CheckApos.idx2apos32_closed_form_equiv.
Print Assumptions CheckApos.CheckApos.idx2apos32_closed_form_equiv.
Locate CheckApos.CheckApos.idx2apos32_injective.
Print Assumptions CheckApos.CheckApos.idx2apos32_injective.
Locate CheckApos.CheckApos.idx2apos32_lower_bound.
Print Assumptions CheckApos.CheckApos.idx2apos32_lower_bound.
Locate CheckApos.CheckApos.idx2apos32_upper_bound.
Print Assumptions CheckApos.CheckApos.idx2apos32_upper_bound.
Locate CheckApos.CheckApos.idx2apos32_fits_32bit.
Print Assumptions CheckApos.CheckApos.idx2apos32_fits_32bit.
Locate CheckApos.CheckApos.nchunks_idempotent.
Print Assumptions CheckApos.CheckApos.nchunks_idempotent.
Locate AssemblyCache.emptyread_id.
Print Assumptions AssemblyCache.emptyread_id.
Locate AssemblyCache.emptywrite_id.
Print Assumptions AssemblyCache.emptywrite_id.

(** * Extracted entry points

    Their assumptions are the FFI shims the OCaml/C++ side has to honour. *)

Locate Nchunks.from_positive.
Print Assumptions Nchunks.from_positive.
Locate Assembly.backup.
Print Assumptions Assembly.backup.
Locate Assembly.finish.
Print Assumptions Assembly.finish.
Locate Assembly.encrypt.
Print Assumptions Assembly.encrypt.
Locate Assembly.decrypt.
Print Assumptions Assembly.decrypt.
Locate Assembly.recall.
Print Assumptions Assembly.recall.
Locate Assembly.restore.
Print Assumptions Assembly.restore.
Locate Assembly.extract.
Print Assumptions Assembly.extract.
Locate EnvironmentWritable.backup.
Print Assumptions EnvironmentWritable.backup.
Locate EnvironmentReadable.restore_assembly.
Print Assumptions EnvironmentReadable.restore_assembly.
Locate Processor.prepare_processor.
Print Assumptions Processor.prepare_processor.
Locate Processor.file_backup.
Print Assumptions Processor.file_backup.
Locate Processor.file_restore.
Print Assumptions Processor.file_restore.
Locate Processor.list_directory_entries.
Print Assumptions Processor.list_directory_entries.
Locate Processor.close.
Print Assumptions Processor.close.
