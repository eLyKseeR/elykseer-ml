(**
      e L y K s e e R
*)

Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String Program.Basics.

From RecordUpdate Require Import RecordUpdate.

From LXR Require Import AssemblyCache.
From LXR Require Import Configuration.
From LXR Require Import Cstdio.
From LXR Require Import Environment.
From LXR Require Import Filesupport.
From LXR Require Import Store.
From LXR Require Import Utilities.

Open Scope N_scope.
Open Scope string_scope.
Open Scope list_scope.

Import ListNotations.

Module Export Processor.

Definition RecordProcessor := Type.
Record processor : RecordProcessor :=
    mkprocessor
        { config : configuration
        ; cache : assemblycache
        }.
(* Print RecordProcessor.
Print processor. *)

Definition cache_sz : positive := 3.
Definition prepare_processor (c : configuration) : processor :=
      {| config := c
       ; cache := prepare_assemblycache c cache_sz
      |}.

Section Methods.

Variable this : processor.

Program Definition update_cache (ac : AssemblyCache.assemblycache) : processor :=
      {| config := this.(config); cache := ac |}.

Program Definition backup_block (wqe : AssemblyCache.writequeueentity) : (list writequeueresult * processor) :=
      match AssemblyCache.enqueue_write_request this.(cache) wqe with
      | (false, _) => (* the queue is full *)
            let (wres, cache') := AssemblyCache.iterate_write_queue this.(cache) in
            match AssemblyCache.enqueue_write_request cache' wqe with
            | (false, _) => (* bad bad *) (nil, this)
            | (true, cache'') => (wres, update_cache cache'')
            end
      | (true, cache') => (nil, update_cache cache')
      end.

Program Definition request_read (rqe : AssemblyCache.readqueueentity) : (list readqueueresult * processor) :=
      match AssemblyCache.enqueue_read_request this.(cache) rqe with
      | (false, _) => (* the queue is full *)
            let (rres, cache') := AssemblyCache.iterate_read_queue this.(cache) in
            match AssemblyCache.enqueue_read_request cache' rqe with
            | (false, _) => (* bad bad *) (nil, this)
            | (true, cache'') => (rres, update_cache cache'')
            end
      | (true, cache') => (nil, update_cache cache')
      end.

Program Definition run_write_requests : (list writequeueresult * processor) :=
      let (wres, cache') := AssemblyCache.iterate_write_queue this.(cache) in
      (wres, update_cache cache').

Program Definition run_read_requests : (list readqueueresult * processor) :=
      let (rres, cache') := AssemblyCache.iterate_read_queue this.(cache) in
      (rres, update_cache cache').

Program Definition get_keys : KeyListStore.R :=
      this.(cache).(acwriteenv).(keys EnvironmentWritable.AB).

Program Definition get_fblocks : FBlockListStore.R :=
      this.(cache).(acwriteenv).(fblocks EnvironmentWritable.AB).

Program Definition close : processor :=
      let ac := AssemblyCache.close this.(cache) in
      update_cache ac.

End Methods.

Section FileProcessor.

Variable this : processor.

Definition block_sz := 32768.

Program Fixpoint r_file_backup_inner (n_blocks : nat) (this : processor) (fi : fileinformation) (fpos : N) (fptr : Cstdio.fptr) (results : list writequeueresult) : (list writequeueresult * processor) :=
      match n_blocks with
      | O => (results, this)
      | S n_blocks' =>
            let dsz : N := fi.(fsize) - fpos in
            let sz : N := if (N.ltb block_sz dsz) then block_sz else dsz in
            let _ := Cstdio.fseek fptr fpos in
            match Cstdio.fread fptr sz with
              | None => (results, this)
              | Some (nread, b) =>
                  let b' := BufferPlain.from_buffer b in
                  let wqe : writequeueentity :=
                        {| qfhash := Utilities.sha256 fi.(fname)
                        ; qfpos := fpos
                        ; qbuffer := b'
                        |} in
                  let (results', this') := backup_block this wqe in
                  r_file_backup_inner n_blocks' this' fi (fpos + sz) fptr (results' ++ results)
            end
      end.
(* Obligations of r_file_backup_inner. *)

Program Definition open_file_backup (n_blocks : N) (fi : fileinformation) (fpos : N) : (list writequeueresult * processor) :=
      match Cstdio.fopen fi.(fname) Cstdio.read_mode with
      | Some fptr =>
            let results := r_file_backup_inner (nat_of_N n_blocks) this fi fpos fptr [] in
            let _ := Cstdio.fclose fptr in
            results
      | None => ([], this)
      end.
(* Obligations of open_file_backup. *)

Program Definition file_backup (fn : filename) : (fileinformation * (list writequeueresult * processor)) :=
      let fi := get_file_information fn in
      let n_blocks := 1 + fi.(fsize) / block_sz in
      let (res1, proc1) := open_file_backup n_blocks fi 0 in
      let (res2, proc2) := run_write_requests proc1 in
      (fi, ((res1 ++ res2), proc2)).

End FileProcessor.


End Processor.
