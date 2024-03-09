(**
      e L y K s e e R
*)

Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String Program.Basics.

From RecordUpdate Require Import RecordUpdate.

From LXR Require Import Assembly.
From LXR Require Import AssemblyCache.
From LXR Require Import Configuration.
From LXR Require Import Cstdio.
From LXR Require Import Environment.
From LXR Require Import Filesupport.
From LXR Require Import Filesystem.
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

Local Program Definition update_cache (ac : AssemblyCache.assemblycache) : processor :=
    {| config := this.(config); cache := ac |}.

Program Definition backup_block (wqe : AssemblyCache.writequeueentity) : processor :=
    match AssemblyCache.enqueue_write_request this.(cache) wqe with
    | (false, _) => (* the queue is full *)
        let (_, cache') := AssemblyCache.iterate_write_queue this.(cache) in
        match AssemblyCache.enqueue_write_request cache' wqe with
        | (false, _) => (* bad bad *) this
        | (true, cache'') => update_cache cache''
        end
    | (true, cache') => update_cache cache'
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
    this.(cache).(ackstore).

Program Definition get_fblocks : FBlockListStore.R :=
    this.(cache).(acfbstore).

Program Definition close : processor :=
    let ac := AssemblyCache.close this.(cache) in
    update_cache ac.

End Methods.

Section FileProcessor.

Variable this : processor.

Definition block_sz := 32768.

Local Program Fixpoint rec_file_backup_inner (n_blocks : nat) (this : processor) (fi : fileinformation) (fpos : N) (fptr : Cstdio.fptr) : processor :=
    match n_blocks with
    | O => this
    | S n_blocks' =>
        if N.ltb fpos fi.(fsize) then
        let dsz : N := fi.(fsize) - fpos in
            let sz : N := if (N.ltb block_sz dsz) then block_sz else dsz in
            let _ := Cstdio.fseek fptr fpos in
            match Cstdio.fread fptr sz with
                | None => this
                | Some (nread, b) =>
                    let b' := BufferPlain.from_buffer b in
                    let wqe : writequeueentity :=
                        {| qfhash := Utilities.sha256 fi.(fname)
                         ; qfpos := fpos
                         ; qbuffer := b'
                        |} in
                    let this' := backup_block this wqe in
                    rec_file_backup_inner n_blocks' this' fi (fpos + sz) fptr
            end
        else this
    end.
(* Obligations of r_file_backup_inner. *)

Local Program Definition open_file_backup (n_blocks : N) (fi : fileinformation) (fpos : N) : processor :=
    match Cstdio.fopen fi.(fname) Cstdio.read_mode with
    | Some fptr =>
        let proc' := rec_file_backup_inner (nat_of_N n_blocks) this fi fpos fptr in
        let _ := Cstdio.fclose fptr in
        proc'
    | None => this
    end.
(* Obligations of open_file_backup. *)

Local Program Definition internal_restore_to (fptr: Cstdio.fptr) (lrres : list readqueueresult) : N :=
    List.fold_left (fun acc rres =>
        match Cstdio.fseek fptr rres.(readrequest).(rqfpos) with
        | None => 0
        | Some fptr' =>
            match Cstdio.fwrite fptr' rres.(readrequest).(rqrlen) (to_buffer rres.(rresult)) with
            | None => 0
            | Some n => n + acc
            end
        end
    ) lrres 0.

Local Program Definition restore_block_to (fptr: Cstdio.fptr) (ac : assemblycache) (block : blockinformation) : N * assemblycache :=
    let rreq := mkreadqueueentity block.(blockaid) block.(blockapos) block.(blocksize) block.(filepos) in
    match enqueue_read_request ac rreq with
    | (false, ac') =>
        let (lrres, ac'') := iterate_read_queue ac' in
        let n := internal_restore_to fptr lrres in
        let (_, ac''') := enqueue_read_request ac'' rreq in
        if N.ltb 0 n then  (* 0 < n *)
            (n, ac''')
        else
            (0, ac''')
    | (true, ac') => (0, ac')
    end.

Local Program Definition restore_file_to (fptr: Cstdio.fptr) (blocks : list blockinformation) : N * assemblycache :=
    let '(res, ac') := List.fold_left (fun '(acc, ac) block =>
            let (n, ac') := restore_block_to fptr ac block in (acc + n, ac')
        ) blocks (0, this.(cache)) in
    let (lrres, ac'') := iterate_read_queue ac' in
    let n := internal_restore_to fptr lrres in
    (n + res, ac'').

Program Definition file_backup (fp : Filesystem.path) : (fileinformation * processor) :=
    let fi := get_file_information (Filesystem.Path.to_string fp) in
    let n_blocks := 1 + (fi.(fsize) + (block_sz / 2) - 1) / block_sz in
    let proc1 := open_file_backup n_blocks fi 0 in
    let (_, proc2) := run_write_requests proc1 in
    (fi, proc2).

Program Definition file_restore (basep : Filesystem.path) (fp : Filesystem.path) (blocks : list blockinformation) : (N * processor) :=
    let targetp := Filesystem.Path.append basep fp in
    if Filesystem.Path.file_exists targetp then
        (0, this)
    else
        let mkdir :=
            if Filesystem.Path.is_directory basep then true
            else Filesystem.create_directories basep in
        match Cstdio.fopen (Filesystem.Path.to_string targetp) Cstdio.write_new_mode with
        | None => (0, this)
        | Some fptr =>
            let '(n, ac') := restore_file_to fptr blocks in
            let proc' := update_cache this ac' in
            match Cstdio.fclose fptr with
            | None => (0, proc')
            | Some _ => (n, proc')
            end
        end.

End FileProcessor.

Section DirectoryProcessor.

Local Program Definition internal_directory_entries (fp : Filesystem.path) : (list Filesystem.path * list Filesystem.path) :=
    Filesystem.list_directory fp ([],[]) (fun de '(lfiles,ldirs) =>
            if Filesystem.Direntry.is_directory de then
                let defp := Filesystem.Direntry.as_path de in
                (lfiles, defp :: ldirs)
            else if Filesystem.Direntry.is_regular_file de then
                let defp := Filesystem.Direntry.as_path de in
                (defp :: lfiles, ldirs)
            else
                (lfiles, ldirs)
        ).

Program Definition directory_backup (this : processor) (fp : Filesystem.path) : (list fileinformation * processor) :=
    let '(lfiles, _) := internal_directory_entries fp in
    List.fold_left (fun '(fis,proc0) filepath => let (fi, proc1) := file_backup proc0 filepath in (fi :: fis, proc1)) lfiles ([], this).

Program Definition directory_backup_0 (this : processor) (fp : Filesystem.path) : processor :=
    let '(lfiles, _) := internal_directory_entries fp in
    List.fold_left (fun proc0 filepath => let (_, proc1) := file_backup proc0 filepath in proc1) lfiles this.

Local Program Fixpoint internal_recursive_backup (maxdepth : nat) (this : processor) (fis0 : list fileinformation) (fp : Filesystem.path) : (list fileinformation * processor) :=
    match maxdepth with
    | O => (fis0, this)
    | S depth =>
        Filesystem.list_directory fp (fis0, this) (fun de '(fis, proc) =>
            if Filesystem.Direntry.is_directory de then
                let defp := Filesystem.Direntry.as_path de in
                let '(fis', proc') := internal_recursive_backup depth proc fis0 defp in
                (fis' ++ fis, proc')
            else if Filesystem.Direntry.is_regular_file de then
                let defp := Filesystem.Direntry.as_path de in
                let '(fi, proc') := file_backup proc defp in
                (fi :: fis, proc')
            else
                (fis, proc)
        )
    end.
Program Definition recursive_backup (this : processor) (maxdepth : N) (fp : Filesystem.path) : (list fileinformation * processor) :=
    if Filesystem.Path.is_directory fp then
        internal_recursive_backup (nat_of_N maxdepth) this [] fp
    else
        ([], this).

Local Program Fixpoint internal_recursive_backup_0 (maxdepth : nat) (this : processor) (fp : Filesystem.path) : processor :=
    match maxdepth with
    | O => this
    | S depth =>
        let '(lfiles, ldirs) := internal_directory_entries fp in
        let proc' := List.fold_left (fun proc0 filepath => let (_, proc1) := file_backup proc0 filepath in proc1) lfiles this in
        List.fold_left (fun proc0 dirpath => internal_recursive_backup_0 depth proc0 dirpath) ldirs proc'
    end.
Program Definition recursive_backup_0 (this : processor) (maxdepth : N) (fp : Filesystem.path) : processor :=
    if Filesystem.Path.is_directory fp then
        internal_recursive_backup_0 (nat_of_N maxdepth) this fp
    else
        this.

End DirectoryProcessor.

End Processor.
