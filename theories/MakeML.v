
Require Coq.extraction.Extraction.
Extraction Language OCaml.

Require Import ZArith NArith.
Open Scope positive_scope.

From LXR Require Import Assembly.
From LXR Require Import AssemblyCache.
From LXR Require Import Store.
From LXR Require Import Cstdio.
From LXR Require Import Configuration.
From LXR Require Import Conversion.
From LXR Require Import Distribution.
From LXR Require Import Environment.
From LXR Require Import Filesupport.
From LXR Require Import Filesystem.
From LXR Require Import Nchunks.
From LXR Require Import Processor.
From LXR Require Import Utilities.
From LXR Require Import Tracer.
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

Extract Constant i2s => "string_of_int".

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
     String.cat x |>
     String.cat (Unix.gethostname ()) |> String.cat (Unix.gettimeofday () |> string_of_float) |>
     Elykseer_crypto.Sha3_256.string
   ".

Extract Constant chunk_identifier =>
   "fun config aid cid ->
      let s = (Configuration.my_id config) ^
              (string_of_int (Conversion.p2i cid)) ^
              aid in
      Elykseer_crypto.Sha3_256.string s
   ".

Extract Constant chunk_identifier_path =>
   "fun config aid cid -> let cident = chunk_identifier config aid cid in
      let subd = Helper.mk_cid_subdir cident in 
      (Configuration.path_chunks config ^ ""/"" ^ subd ^ ""/"" ^ cident ^ "".lxr"")
   ".

Extract Constant Filesystem.path => "Mlcpp_filesystem.Filesystem.path".
Extract Constant Filesystem.Path.to_string => "Mlcpp_filesystem.Filesystem.Path.to_string".
Extract Constant Filesystem.Path.from_string => "Mlcpp_filesystem.Filesystem.Path.from_string".
Extract Constant Filesystem.Path.append => "Mlcpp_filesystem.Filesystem.Path.append".
Extract Constant Filesystem.Path.temp_directory => "Mlcpp_filesystem.Filesystem.Path.temp_directory".
Extract Constant Filesystem.Path.file_exists => "Mlcpp_filesystem.Filesystem.Path.exists".
Extract Constant Filesystem.Path.file_size => "fun p -> Conversion.i2n (Mlcpp_filesystem.Filesystem.Path.file_size p)".
Extract Constant Filesystem.Path.filename => "Mlcpp_filesystem.Filesystem.Path.filename".
Extract Constant Filesystem.Path.extension => "Mlcpp_filesystem.Filesystem.Path.extension".
Extract Constant Filesystem.Path.parent => "Mlcpp_filesystem.Filesystem.Path.parent".
Extract Constant Filesystem.Path.root => "Mlcpp_filesystem.Filesystem.Path.root".
Extract Constant Filesystem.Path.absolute => "Mlcpp_filesystem.Filesystem.Path.absolute".
Extract Constant Filesystem.Path.relative => "Mlcpp_filesystem.Filesystem.Path.relative".
Extract Constant Filesystem.Path.proximate => "Mlcpp_filesystem.Filesystem.Path.proximate".
Extract Constant Filesystem.Path.canonical => "Mlcpp_filesystem.Filesystem.Path.canonical".
Extract Constant Filesystem.Path.weakly_canonical => "Mlcpp_filesystem.Filesystem.Path.weakly_canonical".
Extract Constant Filesystem.Path.path_type => "Mlcpp_filesystem.Filesystem.Path.path_type".
Extract Constant Filesystem.Path.is_regular_file => "Mlcpp_filesystem.Filesystem.Path.is_regular_file".
Extract Constant Filesystem.Path.is_directory => "Mlcpp_filesystem.Filesystem.Path.is_directory".
Extract Constant Filesystem.Path.is_fifo => "Mlcpp_filesystem.Filesystem.Path.is_fifo".
Extract Constant Filesystem.Path.is_block_file => "Mlcpp_filesystem.Filesystem.Path.is_block_file".
Extract Constant Filesystem.Path.is_character_file => "Mlcpp_filesystem.Filesystem.Path.is_character_file".
Extract Constant Filesystem.Path.is_socket => "Mlcpp_filesystem.Filesystem.Path.is_socket".
Extract Constant Filesystem.Path.is_symlink => "Mlcpp_filesystem.Filesystem.Path.is_symlink".
Extract Constant Filesystem.Path.is_other => "Mlcpp_filesystem.Filesystem.Path.is_other".
Extract Constant Filesystem.Permissions.permissions => "Mlcpp_filesystem.Filesystem.Permissions.permissions".
Extract Constant Filesystem.Permissions.get => "Mlcpp_filesystem.Filesystem.Permissions.get".
Extract Constant Filesystem.Permissions.set => "Mlcpp_filesystem.Filesystem.Permissions.set".
Extract Constant Filesystem.Permissions.add => "Mlcpp_filesystem.Filesystem.Permissions.add".
Extract Constant Filesystem.Permissions.remove => "Mlcpp_filesystem.Filesystem.Permissions.remove".
Extract Constant Filesystem.Permissions.to_string => "Mlcpp_filesystem.Filesystem.Permissions.to_string".
Extract Constant Filesystem.Permissions.to_string => "Mlcpp_filesystem.Filesystem.Permissions.to_string".
Extract Constant Filesystem.Permissions.to_dec => "fun p -> Conversion.i2p (Mlcpp_filesystem.Filesystem.Permissions.to_dec p)".
Extract Constant Filesystem.Permissions.to_oct => "fun p -> Conversion.i2p (Mlcpp_filesystem.Filesystem.Permissions.to_oct p)".
Extract Constant Filesystem.Permissions.from_oct => "fun o -> Mlcpp_filesystem.Filesystem.Permissions.from_oct (Conversion.p2i o)".
Extract Constant Filesystem.get_cwd => "Mlcpp_filesystem.Filesystem.get_cwd".
Extract Constant Filesystem.set_cwd => "Mlcpp_filesystem.Filesystem.set_cwd".
Extract Constant Filesystem.copy => "Mlcpp_filesystem.Filesystem.copy".
Extract Constant Filesystem.copy_file => "Mlcpp_filesystem.Filesystem.copy_file".
Extract Constant Filesystem.copy_symlink => "Mlcpp_filesystem.Filesystem.copy_symlink".
Extract Constant Filesystem.create_directory => "Mlcpp_filesystem.Filesystem.create_directory".
Extract Constant Filesystem.create_directories => "Mlcpp_filesystem.Filesystem.create_directories".
Extract Constant Filesystem.create_hard_link => "Mlcpp_filesystem.Filesystem.create_hard_link".
Extract Constant Filesystem.create_symlink => "Mlcpp_filesystem.Filesystem.create_symlink".
Extract Constant Filesystem.create_directory_symlink => "Mlcpp_filesystem.Filesystem.create_directory_symlink".
Extract Constant Filesystem.equivalent => "Mlcpp_filesystem.Filesystem.equivalent".
Extract Constant Filesystem.hard_link_count => "fun p -> Conversion.i2p (Mlcpp_filesystem.Filesystem.hard_link_count p)".
Extract Constant Filesystem.read_symlink => "Mlcpp_filesystem.Filesystem.read_symlink".
Extract Constant Filesystem.remove => "Mlcpp_filesystem.Filesystem.remove".
Extract Constant Filesystem.remove_all => "fun p -> Conversion.i2p (Mlcpp_filesystem.Filesystem.remove_all p)".
Extract Constant Filesystem.rename => "Mlcpp_filesystem.Filesystem.rename".
Extract Constant Filesystem.resize_file => "fun p sz -> Mlcpp_filesystem.Filesystem.resize_file p (Conversion.n2i sz)".
Extract Constant Filesystem.space => "fun p -> List.map (fun i -> Conversion.i2n i) (Mlcpp_filesystem.Filesystem.space p)".
Extract Constant Filesystem.direntry => "Mlcpp_filesystem.Filesystem.direntry".
Extract Constant Filesystem.Direntry.as_path => "Mlcpp_filesystem.Filesystem.Direntry.as_path".
Extract Constant Filesystem.Direntry.direntry_exists => "Mlcpp_filesystem.Filesystem.Direntry.direntry_exists".
Extract Constant Filesystem.Direntry.is_regular_file => "Mlcpp_filesystem.Filesystem.Direntry.is_regular_file".
Extract Constant Filesystem.Direntry.is_block_file => "Mlcpp_filesystem.Filesystem.Direntry.is_block_file".
Extract Constant Filesystem.Direntry.is_character_file => "Mlcpp_filesystem.Filesystem.Direntry.is_character_file".
Extract Constant Filesystem.Direntry.is_directory => "Mlcpp_filesystem.Filesystem.Direntry.is_directory".
Extract Constant Filesystem.Direntry.is_fifo => "Mlcpp_filesystem.Filesystem.Direntry.is_fifo".
Extract Constant Filesystem.Direntry.is_other => "Mlcpp_filesystem.Filesystem.Direntry.is_other".
Extract Constant Filesystem.Direntry.is_socket => "Mlcpp_filesystem.Filesystem.Direntry.is_socket".
Extract Constant Filesystem.Direntry.is_symlink => "Mlcpp_filesystem.Filesystem.Direntry.is_symlink".
Extract Constant Filesystem.Direntry.file_size => "fun de -> Mlcpp_filesystem.Filesystem.Direntry.file_size de |> Conversion.i2n".
Extract Constant Filesystem.Direntry.hard_link_count => "fun de -> Mlcpp_filesystem.Filesystem.Direntry.hard_link_count de |> Conversion.i2n".
Extract Constant Filesystem.list_directory => "Mlcpp_filesystem.Filesystem.list_directory".
(* Extract Constant Filesystem.list_directory =>
   "fun fp ->
      if Mlcpp_filesystem.Filesystem.Path.is_directory fp
      then
         let direntries = ref [] in
         let _ = Mlcpp_filesystem.Filesystem.list_directory fp (fun de ->
            direntries := de :: !direntries) in
         !direntries
      else
         []
   ". *)

Extract Constant Cstdio.fptr => "Mlcpp_cstdio.Cstdio.File.file".
Extract Constant Cstdio.fopen =>
   "fun fname mode ->
      match Mlcpp_cstdio.Cstdio.File.fopen fname mode with
      | Ok fptr -> Some fptr
      | Error (errno, errstr) -> Printf.printf ""fopen '%s' error: %d/%s\n"" fname errno errstr; None
   ".
Extract Constant Cstdio.fclose =>
   "fun fptr ->
      match Mlcpp_cstdio.Cstdio.File.fclose fptr with
      | Ok () -> Some ()
      | Error (errno, errstr) -> Printf.printf ""fclose error: %d/%s\n"" errno errstr; None
   ".
Extract Constant Cstdio.fflush =>
   "fun fptr ->
      match Mlcpp_cstdio.Cstdio.File.fflush fptr with
      | Ok () -> Some fptr
      | Error (errno, errstr) -> Printf.printf ""fflush error: %d/%s\n"" errno errstr; None
   ".
Extract Constant Cstdio.ftell =>
   "fun fptr ->
      match Mlcpp_cstdio.Cstdio.File.ftell fptr with
      | Ok pos -> Some (Conversion.i2n pos)
      | Error (errno, errstr) -> Printf.printf ""ftell error: %d/%s\n"" errno errstr; None
   ".
Extract Constant Cstdio.fseek =>
   "fun fptr pos ->
      match Mlcpp_cstdio.Cstdio.File.fseek fptr (Conversion.n2i pos) with
      | Ok () -> Some fptr
      | Error (errno, errstr) -> Printf.printf ""fseek error: %d/%s\n"" errno errstr; None
   ".
Extract Constant Cstdio.fread =>
   "fun fptr sz ->
      let b = Mlcpp_cstdio.Cstdio.File.Buffer.create (Conversion.n2i sz) in
      match Mlcpp_cstdio.Cstdio.File.fread b (Conversion.n2i sz) fptr with
      | Ok nread -> Some (Conversion.i2n nread, b)
      | Error (errno, errstr) -> Printf.printf ""fread error: %d/%s\n"" errno errstr; None
   ".
Extract Constant Cstdio.fwrite =>
   "fun fptr sz b ->
      match Mlcpp_cstdio.Cstdio.File.fwrite b (Conversion.n2i sz) fptr with
      | Ok nwritten -> Some (Conversion.i2n nwritten)
      | Error (errno, errstr) -> Printf.printf ""fwrite error: %d/%s\n"" errno errstr; None
   ".

Extract Constant Cstdio.cstdio_buffer => "Mlcpp_cstdio.Cstdio.File.Buffer.ta".

Extract Constant BufferEncrypted.buffer_t => "Mlcpp_cstdio.Cstdio.File.Buffer.ta".
Extract Constant BufferEncrypted.buffer_create => "fun n -> Mlcpp_cstdio.Cstdio.File.Buffer.create (Conversion.n2i n)".
Extract Constant BufferEncrypted.buffer_len => "fun b -> Conversion.i2n (Mlcpp_cstdio.Cstdio.File.Buffer.size b)".
Extract Constant BufferEncrypted.calc_checksum => "fun b -> Elykseer_crypto.Sha3_256.buffer b".
Extract Constant BufferEncrypted.from_buffer => "fun b -> Helper.cpp_buffer_id b".
Extract Constant BufferEncrypted.to_buffer => "fun b -> Helper.cpp_buffer_id b".

Extract Constant BufferPlain.buffer_t => "Mlcpp_cstdio.Cstdio.File.Buffer.ta".
Extract Constant BufferPlain.buffer_create => "fun n -> Mlcpp_cstdio.Cstdio.File.Buffer.create (Conversion.n2i n)".
Extract Constant BufferPlain.buffer_len => "fun b -> Conversion.i2n (Mlcpp_cstdio.Cstdio.File.Buffer.size b)".
Extract Constant BufferPlain.calc_checksum => "fun b -> Elykseer_crypto.Sha3_256.buffer b".
Extract Constant BufferPlain.from_buffer => "fun b -> Helper.cpp_buffer_id b".
Extract Constant BufferPlain.to_buffer => "fun b -> Helper.cpp_buffer_id b".

Extract Constant id_buffer_t_from_enc => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_buffer_t_from_full => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_assembly_plain_buffer_t_from_buf => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_assembly_enc_buffer_t_from_buf => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_enc_from_buffer_t => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_assembly_full_buffer_from_writable => "fun b -> Helper.cpp_buffer_id b".
Extract Constant id_assembly_full_ainfo_from_writable => "fun b -> Helper.cpp_buffer_id b".

Extract Constant cpp_encrypt_buffer => "fun b siv spk -> Elykseer_crypto.Aes256.encrypt (Elykseer_crypto.Key128.from_hex siv) (Elykseer_crypto.Key256.from_hex spk) b".
Extract Constant cpp_decrypt_buffer => "fun b siv spk -> Elykseer_crypto.Aes256.decrypt (Elykseer_crypto.Key128.from_hex siv) (Elykseer_crypto.Key256.from_hex spk) b".

Extract Constant cpp_mk_key256 => "fun () -> Elykseer_crypto.Key256.mk () |> Elykseer_crypto.Key256.to_hex".
Extract Constant cpp_mk_key128 => "fun () -> Elykseer_crypto.Key128.mk () |> Elykseer_crypto.Key128.to_hex".
Extract Constant cpp_ranbuf128 => "fun () -> Helper.ranbuf128 ()".

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

Extract Constant get_file_information =>
   "  
    fun (c : Configuration.configuration) fn ->
        { fname = fn;
          fhash = Elykseer_crypto.Sha3_256.string (fn ^ c.my_id);
          fsize = Conversion.i2n (Elykseer_base.Fsutils.fsize fn);
          fowner = string_of_int (Elykseer_base.Fsutils.fowner fn);
          fpermissions = Conversion.i2n (Elykseer_base.Fsutils.fperm fn);
          fmodified = Elykseer_base.Fsutils.fmod fn;
          fchecksum = Elykseer_base.Fsutils.fchksum fn }
   ".

Extract Constant sha3_256 => "Elykseer_crypto.Sha3_256.string".

(*
Axiom messageN : string -> N -> unit.
*)
(* Extract Constant messageN =>
   "
    fun s n -> Printf.printf ""%s %d\n"" s (Conversion.n2i n)
   ".
*)

Extract Constant output_stdout => "fun ll m -> 
   let _ = match ll with
   | Coq_debug -> print_string ""DEBUG ""
   | Coq_info -> print_string ""INFO ""
   | Coq_warning -> print_string ""WARNING ""
   | Coq_error -> print_string ""ERROR ""
   in
   print_endline m; Some ()".

Extract Constant S3Sink.s3push => "fun k _b sink ->
   print_string ""s3push""; print_endline k; sink".
Extract Constant S3Sink.s3pull => "fun k sink ->
   print_string ""s3pull""; print_endline k; (sink, None)".
Extract Constant S3Sink.s3list_n => "fun sink ->
   print_endline ""s3list_n""; (sink, [])".

Extract Constant FSSink.fspush => "fun k _b sink ->
   print_string ""fspush""; print_endline k; sink".
Extract Constant FSSink.fspull => "fun k sink ->
   print_string ""fspull""; print_endline k; (sink, None)".
Extract Constant FSSink.fslist_n => "fun sink ->
   print_endline ""fslist_n""; (sink, [])".

(* extract into "lxr.ml" all named modules and definitions, and their dependencies *)
Extraction "lxr.ml"  Version Conversion Utilities Filesupport Nchunks Assembly
                     Tracer Configuration Environment Cstdio Filesystem
                     AssemblyCache Processor Store Distribution.
