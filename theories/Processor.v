(**
      e L y K s e e R
*)

Require Import NArith PArith.
From Coq Require Import NArith.BinNat Lists.List Strings.String Bool.Bool Program.Basics.

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
        let cache' := AssemblyCache.iterate_write_queue this.(cache) in
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

Program Definition run_write_requests : processor :=
    let cache' := AssemblyCache.iterate_write_queue this.(cache) in
    update_cache cache'.

Program Definition run_read_requests : (list readqueueresult * processor) :=
    let (rres, cache') := AssemblyCache.iterate_read_queue this.(cache) in
    (rres, update_cache cache').

(* Program Definition get_keys : KeyListStore.R :=
    this.(cache).(ackstore). *)

(* Program Definition get_fblocks : FBlockListStore.R :=
    this.(cache).(acfbstore). *)

Program Definition close : processor :=
    let ac := AssemblyCache.close this.(cache) in
    update_cache ac.

End Methods.

Section FileProcessor.

Variable this : processor.

Definition block_sz := 32768.

Local Program Fixpoint rec_file_backup_inner0 (n_blocks : nat) (this : processor) (fi : fileinformation) (fpos : N) (fptr : Cstdio.fptr) : processor :=
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
                        {| qfhash := fi.(fhash)
                         ; qfpos := fpos
                         ; qbuffer := b'
                        |} in
                    let this' := backup_block this wqe in
                    rec_file_backup_inner0 n_blocks' this' fi (fpos + sz) fptr
            end
        else this
    end.
Local Program Fixpoint rec_file_backup_inner (tgtfbs : list blockinformation) (this : processor) (fhash : string) (fptr : Cstdio.fptr) : processor :=
    match tgtfbs with
    | nil => this
    | fb :: tgtfbs' =>
    (*
      begin
        Printf.printf "  block %d %d @ %d" (Conversion.p2i fb.Assembly.blockid) (Conversion.n2i fb.Assembly.blocksize) (Conversion.n2i fb.Assembly.filepos);
      end;
    *)
        let ofptr' := Cstdio.fseek fptr fb.(filepos) in
        match ofptr' with
            | None => this 
            | Some fptr' =>
            match Cstdio.fread fptr fb.(blocksize) with
                | None => this
                | Some (nread, b) =>
                    let b' := BufferPlain.from_buffer b in
                    (* compare *)
                    let found := if (fb.(bchecksum) =? "")
                        then false
                        else
                            let chksum := BufferPlain.calc_checksum b' in
                            chksum =? fb.(bchecksum)
                    in
                    if (found)
                    then
                        rec_file_backup_inner tgtfbs' this fhash fptr'
                    else
                        let wqe : writequeueentity :=
                            {| qfhash := fhash
                            ; qfpos := fb.(filepos)
                            ; qbuffer := b'
                            |} in
                        let this' := backup_block this wqe in
                        rec_file_backup_inner tgtfbs' this' fhash fptr'
            end
        end
    end.


Local Program Definition open_file_backup0 (n_blocks : N) (fi : fileinformation) (fpos : N) : processor :=
    match Cstdio.fopen fi.(fname) Cstdio.read_mode with
    | Some fptr =>
        let proc' := rec_file_backup_inner0 (nat_of_N n_blocks) this fi fpos fptr in
        match Cstdio.fclose fptr with
        | None => proc'
        | Some _ =>
            update_cache proc' (AssemblyCache.add_fileinformation proc'.(cache) fi)
        end
    | None => this
    end.
Local Program Definition open_file_backup (fi : fileinformation) (tgtfbs : list blockinformation) : processor :=
    match Cstdio.fopen fi.(fname) Cstdio.read_mode with
    | Some fptr =>
        let proc' := rec_file_backup_inner tgtfbs this fi.(fhash) fptr in
        match Cstdio.fclose fptr with
        | None => proc'
        | Some _ =>
            update_cache proc' (AssemblyCache.add_fileinformation proc'.(cache) fi)
        end
    | None => this
    end.

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

Local Program Fixpoint prepare_blocks' (fuel : nat) (bid : positive) (fpos : N) (fsz : N) (agg : list blockinformation) : list blockinformation :=
    match fuel with
    | O => List.rev agg
    | S fuel' =>
        let bsz := if (block_sz <? fsz)%N then block_sz else fsz in
        let blocks' := if (0 <? bsz)%N
            then
                {| blockid := bid; bchecksum := ""; blocksize := bsz; filepos := fpos; blockaid := ""; blockapos := 0 |} :: agg
            else
                agg
        in
        prepare_blocks' fuel' (bid + 1) (fpos + bsz) (fsz - bsz) blocks'
    end.

Local Program Fixpoint merge_blocks (newbs : list blockinformation) (curbs : list blockinformation) (agg : list blockinformation) : list blockinformation :=
    match newbs with
    | nil => List.rev agg
    | h :: r =>
        let '(newb,r') := match curbs with
        | nil => (h,nil)
        | h' :: r' =>
            if ((h.(blocksize) =? h'.(blocksize))%N && (h.(filepos) =? h'.(filepos))%N)
            then ({| blockid := h.(blockid); bchecksum := h'.(bchecksum); blocksize := h'.(blocksize);
                     filepos := h'.(filepos); blockaid := h'.(blockaid); blockapos := h'.(blockapos); |}, r')
            else (h,r')
        end in
        merge_blocks r r' (newb :: agg)
    end.

Local Program Definition prepare_blocks (curbs : list blockinformation) (fsz : N) : list blockinformation :=
    let n_blocks := 1 + (fsz + (block_sz / 2) - 1) / block_sz in
    let newbs := prepare_blocks' (N.to_nat n_blocks) 1 0 fsz [] in
    match curbs with
        | nil => newbs
        | _ => merge_blocks newbs curbs []
    end.

Program Definition file_backup (find_fchecksum : string -> option string) (find_fblocks : string -> list blockinformation) (fp : Filesystem.path) : processor :=
    let fn := Filesystem.Path.to_string fp in
    let fi := get_file_information this.(config) fn in
    (* deduplication level 1: compare file checksums *)
    (* let ofi' := FileinformationStore.find fi.(fhash) this.(cache).(acfistore) in
    let found :=
        match ofi' with
        | None => false
        | Some fi' => if fi'.(fchecksum) =? fi.(fchecksum) then true else false
        end *)
    let found :=
        match find_fchecksum fi.(fhash) with
        | None => false
        | Some fchecksum' => fchecksum' =? fi.(fchecksum)
        end
    in
    if (found) then
        this
    else
        (* let curbs := FBlockListStore.find_all fi.(fhash) this.(cache).(acfbstore) in *)
        let curbs := find_fblocks fi.(fhash) in
        let tgtbs := prepare_blocks curbs fi.(fsize) in
        (* deduplication level 2 per block *)
        let proc1 := open_file_backup fi tgtbs in
        let proc2 := run_write_requests proc1 in
        proc2.

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

Program Definition directory_backup (this : processor) (fp : Filesystem.path) : processor :=
    let '(lfiles, _) := internal_directory_entries fp in
    List.fold_left (fun proc fn => file_backup proc (const None) (const []) fn) lfiles this.

Local Program Fixpoint internal_recursive_backup (maxdepth : nat) (this : processor) (fp : Filesystem.path) : processor :=
    match maxdepth with
    | O => this
    | S depth =>
        Filesystem.list_directory fp this (fun de proc =>
            if Filesystem.Direntry.is_directory de then
                let defp := Filesystem.Direntry.as_path de in
                internal_recursive_backup depth proc defp
            else if Filesystem.Direntry.is_regular_file de then
                let defp := Filesystem.Direntry.as_path de in
                file_backup proc (const None) (const []) defp
            else
                proc
        )
    end.
Program Definition recursive_backup (this : processor) (maxdepth : N) (fp : Filesystem.path) : processor :=
    if Filesystem.Path.is_directory fp then
        internal_recursive_backup (nat_of_N maxdepth) this fp
    else
        this.

End DirectoryProcessor.

End Processor.
