
Require Coq.extraction.Extraction.
Extraction Language OCaml.

Require Import ZArith NArith.
Open Scope positive_scope.

From LXR Require Import Assembly.
From LXR Require Import BackupPlanner.
From LXR Require Import Buffer.
From LXR Require Import Configuration.
From LXR Require Import Conversion.
From LXR Require Import Environment.
From LXR Require Import Filesupport.
From LXR Require Import Filetypes.
From LXR Require Import Nchunks.
From LXR Require Import Utilities.
From LXR Require Import Version.

From Coq Require Import ExtrOcamlBasic.
From Coq Require Import ExtrOcamlNativeString.


Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inductive sumbool => "bool" ["true" "false"].
Extract Inductive option => option [ Some None ].

(** the following were found in: https://github.com/coq-contribs/zchinese 
    and are very useful to convert standard OCaml 'int' into positive|N|Z *)

Extract Inlined Constant int => "int".

Extract Constant i2p =>
   "  
    let rec i2p = function 
       1 -> XH 
     | n -> let n' = i2p (n/2) in if (n mod 2)=0 then XO n' else XI n'
     in i2p
   ".
 
Extract Constant p2i =>
   "
    let rec p2i = function 
       XH -> 1
     | XO p -> 2*(p2i p)
     | XI p -> 2*(p2i p)+1
     in p2i 
   ".

Extract Constant i2z =>
   "
    function 
      0 -> Z0
    | n -> if n < 0 then Zneg (i2p (-n)) else Zpos (i2p n)
   ".

Extract Constant z2i =>
   "
    function
      Z0 -> 0 
    | Zpos p -> p2i p
    | Zneg p -> -(p2i p)
   ".

Extract Constant i2n =>
   "
    function 
      0 -> N0
    | n -> Npos (i2p n)
   ".

Extract Constant n2i =>
   "
    function
      N0 -> 0 
    | Npos p -> p2i p
   ".

(* Extract Constant rndsetup =>
   "
    function
     _ -> Random.self_init (); Conversion.i2n 0
   ". *)

Extract Constant rnd =>
   "
    function
     _ -> Elykseer_crypto.Random.with_rng (fun rng -> Elykseer_crypto.Random.random32 rng) |> Conversion.i2n
   ".

Extract Constant rnd256 =>
   "
   function
   x -> Elykseer_crypto.Random.with_rng (fun rng -> Elykseer_crypto.Random.random32 rng) |> string_of_int |>
     String.cat (Conversion.n2i x |> string_of_int) |>
     String.cat (Unix.gethostname ()) |> String.cat (Unix.gettimeofday () |> string_of_float) |>
     Elykseer_crypto.Sha256.string
   ".

Extract Constant chunk_identifier =>
   "  
    fun config aid cid -> let s =
      (string_of_int (Conversion.n2i (Configuration.my_id config))) ^
      (string_of_int (Conversion.p2i cid)) ^
      aid in
      (* Printf.eprintf ""c id: %s\n"" s; *)
      Elykseer_base.Hashing.sha256 s
   ".

Extract Constant chunk_identifier_path =>
   "  
    fun config aid cid -> let cident = chunk_identifier config aid cid in
      let subd = Helper.mk_cid_subdir cident in 
      (Configuration.path_chunks config ^ ""/"" ^ subd ^ ""/"" ^ cident ^ "".lxr"")
   ".

Extract Constant Buffer.cstdio_buffer => "Mlcpp_cstdio.Cstdio.File.Buffer.ta".

Extract Constant BufferEncrypted.buffer_t => "Mlcpp_cstdio.Cstdio.File.Buffer.ta".
Extract Constant BufferEncrypted.buffer_create => "fun n -> Mlcpp_cstdio.Cstdio.File.Buffer.create (Conversion.n2i n)".
Extract Constant BufferEncrypted.buffer_len => "fun b -> Conversion.i2n (Mlcpp_cstdio.Cstdio.File.Buffer.size b)".
Extract Constant BufferEncrypted.calc_checksum => "fun b -> Elykseer_base.Buffer.sha256 b".
Extract Constant BufferEncrypted.from_buffer => "fun b -> Helper.cpp_buffer_id b".
Extract Constant BufferEncrypted.to_buffer => "fun b -> Helper.cpp_buffer_id b".

Extract Constant BufferPlain.buffer_t => "Mlcpp_cstdio.Cstdio.File.Buffer.ta".
Extract Constant BufferPlain.buffer_create => "fun n -> Mlcpp_cstdio.Cstdio.File.Buffer.create (Conversion.n2i n)".
Extract Constant BufferPlain.buffer_len => "fun b -> Conversion.i2n (Mlcpp_cstdio.Cstdio.File.Buffer.size b)".
Extract Constant BufferPlain.calc_checksum => "fun b -> Elykseer_base.Buffer.sha256 b".
Extract Constant BufferPlain.from_buffer => "fun b -> Helper.cpp_buffer_id b".
Extract Constant BufferPlain.to_buffer => "fun b -> Helper.cpp_buffer_id b".

Extract Constant id_buffer_t_from_enc => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_buffer_t_from_full => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_buffer_t_from_writable => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_assembly_plain_buffer_t_from_buf => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_assembly_enc_buffer_t_from_buf => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_enc_from_buffer_t => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_assembly_full_buffer_from_writable => "fun b -> Helper.cpp_buffer_id b".

Extract Constant cpp_encrypt_buffer => "fun b pw -> Helper.cpp_encrypt_buffer b pw".  (* TODO *)
Extract Constant cpp_decrypt_buffer => "fun b pw -> Helper.cpp_decrypt_buffer b pw".  (* TODO *)

Extract Constant assembly_add_content => (* BufferPlain.buffer_t -> N -> N -> AssemblyPlainWritable.B -> N. *)
   "
    fun src sz_N pos_N tgt ->
      let sz = Conversion.n2i sz_N
      and pos = Conversion.n2i pos_N in
      Elykseer_base.Assembly.add_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt |> Conversion.i2n
   ".

Extract Constant assembly_get_content => (* AssemblyPlainWritable.B -> N -> N -> BufferPlain.buffer_t -> N. *)
   "
    fun src sz_N pos_N tgt ->
      let sz = Conversion.n2i sz_N
      and pos = Conversion.n2i pos_N in
      Elykseer_base.Assembly.get_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt |> Conversion.i2n
   ".

Extract Constant BufferEncrypted.copy_sz_pos =>
   "
    fun bsrc npos1 nsz btgt npos2 -> Conversion.i2n @@
      Mlcpp_cstdio.Cstdio.File.Buffer.copy_sz_pos bsrc ~pos1:(Conversion.n2i npos1) ~sz:(Conversion.n2i nsz) btgt ~pos2:(Conversion.n2i npos2)
   ".

Extract Constant BufferPlain.copy_sz_pos =>
   "
    fun bsrc npos1 nsz btgt npos2 -> Conversion.i2n @@
      Mlcpp_cstdio.Cstdio.File.Buffer.copy_sz_pos bsrc ~pos1:(Conversion.n2i npos1) ~sz:(Conversion.n2i nsz) btgt ~pos2:(Conversion.n2i npos2)
   ".

Extract Constant ext_store_chunk_to_path =>
   "
    fun fp nsz npos b -> Conversion.i2n @@
      Helper.store_chunk_to_path (Mlcpp_filesystem.Filesystem.Path.from_string fp) (Conversion.n2i nsz) (Conversion.n2i npos) b
   ".

Extract Constant ext_load_chunk_from_path =>
   "
    fun fp -> Helper.load_chunk_from_path (Mlcpp_filesystem.Filesystem.Path.from_string fp)
   ".

Extract Constant get_file_information =>
   "  
    fun fn ->
        { Filetypes.fname = fn;
          Filetypes.fsize = Conversion.i2n (Elykseer_base.Fsutils.fsize fn);
          Filetypes.fowner = string_of_int (Elykseer_base.Fsutils.fowner fn);
          Filetypes.fpermissions = Conversion.i2n (Elykseer_base.Fsutils.fperm fn);
          Filetypes.fmodified = Elykseer_base.Fsutils.fmod fn;
          Filetypes.fchecksum = Elykseer_base.Fsutils.fchksum fn }
   ".

(* extract into "lxr.ml" all named modules and definitions, and their dependencies *)
Extraction "lxr.ml"  Version Conversion Utilities Filetypes Filesupport Nchunks Assembly Configuration Environment Buffer BackupPlanner.
