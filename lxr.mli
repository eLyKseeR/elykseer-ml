
val negb : bool -> bool

type nat =
| O
| S of nat

val fst : ('a1 * 'a2) -> 'a1

val length : 'a1 list -> nat

val app : 'a1 list -> 'a1 list -> 'a1 list

type comparison =
| Eq
| Lt
| Gt

val add : nat -> nat -> nat

val eqb : nat -> nat -> bool

val leb : nat -> nat -> bool

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos :
 sig
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg
 end

module Coq_Pos :
 sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  val pred_double : positive -> positive

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  val succ_double_mask : mask -> mask

  val double_mask : mask -> mask

  val double_pred_mask : positive -> mask

  val sub_mask : positive -> positive -> mask

  val sub_mask_carry : positive -> positive -> mask

  val mul : positive -> positive -> positive

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val min : positive -> positive -> positive

  val max : positive -> positive -> positive

  val eqb : positive -> positive -> bool

  val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1

  val to_nat : positive -> nat

  val of_nat : nat -> positive

  val of_succ_nat : nat -> positive
 end

module N :
 sig
  val succ_double : n -> n

  val double : n -> n

  val add : n -> n -> n

  val sub : n -> n -> n

  val mul : n -> n -> n

  val compare : n -> n -> comparison

  val eqb : n -> n -> bool

  val leb : n -> n -> bool

  val ltb : n -> n -> bool

  val pos_div_eucl : positive -> n -> n * n

  val div_eucl : n -> n -> n * n

  val div : n -> n -> n

  val to_nat : n -> nat

  val of_nat : nat -> n
 end

val removelast : 'a1 list -> 'a1 list

val rev : 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1

val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list

val seq : nat -> nat -> nat list

module Conversion :
 sig
  val pos2N : positive -> n

  val nat2N : nat -> n

  val i2p : int -> positive

  val p2i : positive -> int

  val i2z : int -> z

  val z2i : z -> int

  val i2n : int -> n

  val n2i : n -> int

  val i2s : int -> string
 end

module Nchunks :
 sig
  val max_n : positive

  val min_n : positive

  module Private :
   sig
    type t = positive

    val from_positive : positive -> t

    val from_int : int -> t

    val to_positive : t -> positive

    val to_N : t -> n
   end

  type t = Private.t

  val from_positive : positive -> Private.t

  val from_int : int -> Private.t

  val to_positive : Private.t -> positive

  val to_N : Private.t -> n
 end

module Cstdio :
 sig
  type coq_EncryptionState =
  | Plain
  | Encrypted

  val coq_EncryptionState_rect : 'a1 -> 'a1 -> coq_EncryptionState -> 'a1

  val coq_EncryptionState_rec : 'a1 -> 'a1 -> coq_EncryptionState -> 'a1

  type cstdio_buffer = Mlcpp_cstdio.Cstdio.File.Buffer.ta

  type mode = string

  val read_mode : mode

  val write_mode : mode

  val write_new_mode : mode

  val append_mode : mode

  type fptr = Mlcpp_cstdio.Cstdio.File.file

  val fopen : string -> mode -> fptr option

  val fclose : fptr -> unit option

  val fflush : fptr -> fptr option

  val fread : fptr -> n -> (n * cstdio_buffer) option

  val fwrite : fptr -> n -> cstdio_buffer -> n option

  val ftell : fptr -> n option

  val fseek : fptr -> n -> fptr option

  module type BUF =
   sig
    type buffer_t

    val buffer_create : n -> buffer_t

    val buffer_len : buffer_t -> n

    val calc_checksum : buffer_t -> string

    val copy_sz_pos : buffer_t -> n -> n -> buffer_t -> n -> n

    val from_buffer : cstdio_buffer -> buffer_t

    val to_buffer : buffer_t -> cstdio_buffer

    val state : coq_EncryptionState
   end

  module BufferEncrypted :
   BUF

  module BufferPlain :
   BUF

  val cpp_encrypt_buffer :
    BufferPlain.buffer_t -> string -> string -> BufferEncrypted.buffer_t

  val encrypt :
    BufferPlain.buffer_t -> string -> string -> BufferEncrypted.buffer_t

  val cpp_decrypt_buffer :
    BufferEncrypted.buffer_t -> string -> string -> BufferPlain.buffer_t

  val decrypt :
    BufferEncrypted.buffer_t -> string -> string -> BufferPlain.buffer_t

  val cpp_ranbuf128 : unit -> cstdio_buffer

  val ranbuf128 : unit -> BufferPlain.buffer_t
 end

module Tracer :
 sig
  type loglevel =
  | Coq_debug
  | Coq_info
  | Coq_warning
  | Coq_error

  val loglevel_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> loglevel -> 'a1

  val loglevel_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> loglevel -> 'a1

  type tracer = { logDebug : (string -> unit option);
                  logInfo : (string -> unit option);
                  logWarning : (string -> unit option);
                  logError : (string -> unit option) }

  val logDebug : tracer -> string -> unit option

  val logInfo : tracer -> string -> unit option

  val logWarning : tracer -> string -> unit option

  val logError : tracer -> string -> unit option

  val ignore : 'a1 -> unit option

  val nullTracer : tracer

  val output_stdout : loglevel -> string -> unit option

  val stdoutTracerDebug : tracer

  val stdoutTracerInfo : tracer

  val stdoutTracerWarning : tracer

  val stdoutTracerError : tracer

  val log : tracer -> loglevel -> string -> unit option

  val conditionalTrace :
    tracer -> bool -> loglevel -> string option -> (unit -> 'a1 option) ->
    loglevel -> string option -> (unit -> 'a1 option) -> 'a1 option

  val optionalTrace :
    tracer -> 'a2 option -> loglevel -> string option -> (unit -> 'a1 option)
    -> loglevel -> string option -> ('a2 -> 'a1 option) -> 'a1 option
 end

module Configuration :
 sig
  type configuration = { config_nchunks : Nchunks.t; path_chunks : string;
                         path_db : string; my_id : string;
                         trace : Tracer.tracer }

  val config_nchunks : configuration -> Nchunks.t

  val path_chunks : configuration -> string

  val path_db : configuration -> string

  val my_id : configuration -> string

  val trace : configuration -> Tracer.tracer
 end

module Filesystem :
 sig
  type path = Mlcpp_filesystem.Filesystem.path

  module Path :
   sig
    val to_string : path -> string

    val from_string : string -> path

    val append : path -> path -> path

    val temp_directory : unit -> path

    val file_exists : path -> bool

    val file_size : path -> n

    val filename : path -> path

    val extension : path -> path

    val parent : path -> path

    val root : path -> path

    val absolute : path -> path option

    val relative : path -> path option

    val proximate : path -> path option

    val canonical : path -> path option

    val weakly_canonical : path -> path option

    val path_type : path -> string

    val is_regular_file : path -> bool

    val is_directory : path -> bool

    val is_fifo : path -> bool

    val is_block_file : path -> bool

    val is_character_file : path -> bool

    val is_socket : path -> bool

    val is_symlink : path -> bool

    val is_other : path -> bool
   end

  module Permissions :
   sig
    type permissions = Mlcpp_filesystem.Filesystem.Permissions.permissions

    val get : path -> permissions option

    val set : path -> permissions -> bool

    val add : path -> permissions -> bool

    val remove : path -> permissions -> bool

    val to_string : permissions -> string

    val to_dec : permissions -> positive

    val to_oct : permissions -> positive

    val from_oct : positive -> permissions
   end

  val get_cwd : unit -> path

  val set_cwd : path -> bool

  val copy : path -> path -> bool

  val copy_file : path -> path -> bool

  val copy_symlink : path -> path -> bool

  val create_directory : path -> bool

  val create_directories : path -> bool

  val create_hard_link : path -> path -> bool

  val create_symlink : path -> path -> bool

  val create_directory_symlink : path -> path -> bool

  val equivalent : path -> path -> bool

  val hard_link_count : path -> positive

  val read_symlink : path -> path option

  val remove : path -> bool

  val remove_all : path -> positive

  val rename : path -> path -> bool

  val resize_file : path -> n -> bool

  val space : path -> n list

  type direntry = Mlcpp_filesystem.Filesystem.direntry

  module Direntry :
   sig
    val as_path : direntry -> path

    val as_string : direntry -> string

    val direntry_exists : direntry -> bool

    val is_regular_file : direntry -> bool

    val is_block_file : direntry -> bool

    val is_character_file : direntry -> bool

    val is_directory : direntry -> bool

    val is_fifo : direntry -> bool

    val is_other : direntry -> bool

    val is_socket : direntry -> bool

    val is_symlink : direntry -> bool

    val file_size : direntry -> n

    val hard_link_count : direntry -> n
   end

  val list_directory : path -> 'a1 -> (direntry -> 'a1 -> 'a1) -> 'a1
 end

module Utilities :
 sig
  val make_list : positive -> positive list

  val rnd : n -> n

  val rnd256 : string -> string

  val sha256 : string -> string
 end

module Assembly :
 sig
  val chunkwidth : positive

  val chunklength : positive

  val chunksize : positive

  val chunksize_N : n

  val assemblysize : Nchunks.Private.t -> n

  type aid_t = string

  val mkaid : Configuration.configuration -> aid_t

  type assemblyinformation = { nchunks : Nchunks.Private.t; aid : aid_t;
                               apos : n }

  val nchunks : assemblyinformation -> Nchunks.Private.t

  val aid : assemblyinformation -> aid_t

  val apos : assemblyinformation -> n

  type keyinformation = { ivec : string; pkey : string; localid : string;
                          localnchunks : positive }

  val ivec : keyinformation -> string

  val pkey : keyinformation -> string

  val localid : keyinformation -> string

  val localnchunks : keyinformation -> positive

  type blockinformation = { blockid : positive; bchecksum : string;
                            blocksize : n; filepos : n; blockaid : aid_t;
                            blockapos : n }

  val blockid : blockinformation -> positive

  val bchecksum : blockinformation -> string

  val blocksize : blockinformation -> n

  val filepos : blockinformation -> n

  val blockaid : blockinformation -> aid_t

  val blockapos : blockinformation -> n

  module type ASS =
   sig
    type coq_B

    val create : Configuration.configuration -> assemblyinformation * coq_B

    val buffer_len : coq_B -> n

    val calc_checksum : coq_B -> string
   end

  module AssemblyPlainWritable :
   ASS

  module AssemblyEncrypted :
   ASS

  module AssemblyPlainFull :
   ASS

  val set_apos : assemblyinformation -> n -> assemblyinformation

  val id_assembly_full_ainfo_from_writable :
    assemblyinformation -> assemblyinformation

  val id_assembly_full_buffer_from_writable :
    AssemblyPlainWritable.coq_B -> AssemblyPlainFull.coq_B

  val finish :
    assemblyinformation -> AssemblyPlainWritable.coq_B ->
    assemblyinformation * AssemblyPlainFull.coq_B

  val assembly_add_content :
    Cstdio.BufferPlain.buffer_t -> n -> n -> AssemblyPlainWritable.coq_B -> n

  val backup :
    assemblyinformation -> AssemblyPlainWritable.coq_B -> n ->
    Cstdio.BufferPlain.buffer_t -> assemblyinformation * blockinformation

  val id_buffer_t_from_full :
    AssemblyPlainFull.coq_B -> Cstdio.BufferPlain.buffer_t

  val id_assembly_enc_buffer_t_from_buf :
    Cstdio.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B

  val encrypt :
    assemblyinformation -> AssemblyPlainFull.coq_B -> keyinformation ->
    (assemblyinformation * AssemblyEncrypted.coq_B) option

  val assembly_get_content :
    AssemblyPlainFull.coq_B -> n -> n -> Cstdio.BufferPlain.buffer_t -> n

  val restore :
    AssemblyPlainFull.coq_B -> blockinformation ->
    Cstdio.BufferPlain.buffer_t option

  val id_buffer_t_from_enc :
    AssemblyEncrypted.coq_B -> Cstdio.BufferEncrypted.buffer_t

  val id_assembly_plain_buffer_t_from_buf :
    Cstdio.BufferPlain.buffer_t -> AssemblyPlainFull.coq_B

  val decrypt :
    assemblyinformation -> AssemblyEncrypted.coq_B -> keyinformation ->
    (assemblyinformation * AssemblyPlainFull.coq_B) option

  val chunk_identifier :
    Configuration.configuration -> aid_t -> positive -> string

  val chunk_identifier_path :
    Configuration.configuration -> aid_t -> positive -> string

  val load_chunk_from_path : string -> Cstdio.BufferEncrypted.buffer_t option

  val id_enc_from_buffer_t :
    Cstdio.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B

  val recall :
    Configuration.configuration -> assemblyinformation ->
    (assemblyinformation * AssemblyEncrypted.coq_B) option

  val store_chunk_to_path :
    string -> n -> n -> Cstdio.BufferEncrypted.buffer_t -> n

  val extract :
    Configuration.configuration -> assemblyinformation ->
    AssemblyEncrypted.coq_B -> n
 end

module Filesupport :
 sig
  type filename = string

  type fileinformation = { fname : filename; fhash : string; fsize : 
                           n; fowner : string; fpermissions : n;
                           fmodified : string; fchecksum : string }

  val fname : fileinformation -> filename

  val fhash : fileinformation -> string

  val fsize : fileinformation -> n

  val fowner : fileinformation -> string

  val fpermissions : fileinformation -> n

  val fmodified : fileinformation -> string

  val fchecksum : fileinformation -> string

  val get_file_information :
    Configuration.configuration -> filename -> fileinformation
 end

module Store :
 sig
  type 'kVs store = { sconfig : Configuration.configuration; entries : 'kVs }

  val sconfig : 'a1 store -> Configuration.configuration

  val entries : 'a1 store -> 'a1

  val rec_find : string -> (string * 'a1) list -> 'a1 option

  val rec_find_all : string -> (string * 'a1) list -> 'a1 list -> 'a1 list

  module type STORE =
   sig
    type coq_K

    type coq_V

    type coq_KVs

    type coq_R

    val init : Configuration.configuration -> coq_R

    val add : coq_K -> coq_V -> coq_R -> coq_R

    val find : coq_K -> coq_R -> coq_V option

    val find_all : coq_K -> coq_R -> coq_V list
   end

  module KeyListStore :
   sig
    type coq_K = Assembly.aid_t

    type coq_V = Assembly.keyinformation

    type coq_KVs = (coq_K * coq_V) list

    type coq_R = coq_KVs store

    val init : Configuration.configuration -> coq_R

    val add : coq_K -> coq_V -> coq_R -> coq_R

    val find : coq_K -> coq_R -> coq_V option

    val find_all : coq_K -> coq_R -> coq_V list
   end

  module FBlockListStore :
   sig
    type coq_K = string

    type coq_V = Assembly.blockinformation

    type coq_KVs = (coq_K * coq_V) list

    type coq_R = coq_KVs store

    val init : Configuration.configuration -> coq_R

    val add : coq_K -> coq_V -> coq_R -> coq_R

    val find : coq_K -> coq_R -> coq_V option

    val find_all : coq_K -> coq_R -> coq_V list
   end

  module FileinformationStore :
   sig
    type coq_K = string

    type coq_V = Filesupport.fileinformation

    type coq_KVs = (coq_K * coq_V) list

    type coq_R = coq_KVs store

    val init : Configuration.configuration -> coq_R

    val add : coq_K -> coq_V -> coq_R -> coq_R

    val find : coq_K -> coq_R -> coq_V option

    val find_all : coq_K -> coq_R -> coq_V list
   end
 end

module Environment :
 sig
  type 'aB environment = { cur_assembly : Assembly.assemblyinformation;
                           cur_buffer : 'aB;
                           econfig : Configuration.configuration }

  val cur_assembly : 'a1 environment -> Assembly.assemblyinformation

  val cur_buffer : 'a1 environment -> 'a1

  val econfig : 'a1 environment -> Configuration.configuration

  val cpp_mk_key256 : unit -> string

  val cpp_mk_key128 : unit -> string

  module type ENV =
   sig
    type coq_AB

    type coq_E = coq_AB environment

    val initial_environment : Configuration.configuration -> coq_E
   end

  module EnvironmentWritable :
   sig
    type coq_AB = Assembly.AssemblyPlainWritable.coq_B

    type coq_E = coq_AB environment

    val initial_environment : Configuration.configuration -> coq_E

    val recreate_assembly : coq_AB environment -> coq_AB environment

    val finalise_assembly :
      coq_AB environment -> (Assembly.aid_t * Assembly.keyinformation) option

    val finalise_and_recreate_assembly :
      coq_AB environment -> (coq_AB
      environment * (Assembly.aid_t * Assembly.keyinformation)) option

    val backup :
      coq_AB environment -> string -> n -> Cstdio.BufferPlain.buffer_t ->
      coq_AB
      environment * (Assembly.blockinformation * (Assembly.aid_t * Assembly.keyinformation)
      option)
   end

  module EnvironmentReadable :
   sig
    type coq_AB = Assembly.AssemblyPlainFull.coq_B

    type coq_E = coq_AB environment

    val initial_environment : Configuration.configuration -> coq_E

    val restore_assembly :
      coq_AB environment -> Assembly.aid_t -> Assembly.keyinformation ->
      coq_AB environment option
   end
 end

module AssemblyCache :
 sig
  type readqueueentity = { rqaid : Assembly.aid_t; rqapos : n; rqrlen : 
                           n; rqfpos : n }

  val rqaid : readqueueentity -> Assembly.aid_t

  val rqapos : readqueueentity -> n

  val rqrlen : readqueueentity -> n

  val rqfpos : readqueueentity -> n

  type readqueueresult = { readrequest : readqueueentity;
                           rresult : Cstdio.BufferPlain.buffer_t }

  val readrequest : readqueueresult -> readqueueentity

  val rresult : readqueueresult -> Cstdio.BufferPlain.buffer_t

  type writequeueentity = { qfhash : string; qfpos : n;
                            qbuffer : Cstdio.BufferPlain.buffer_t }

  val qfhash : writequeueentity -> string

  val qfpos : writequeueentity -> n

  val qbuffer : writequeueentity -> Cstdio.BufferPlain.buffer_t

  val qsize : positive

  type readqueue = { rqueue : readqueueentity list; rqueuesz : positive }

  val rqueue : readqueue -> readqueueentity list

  val rqueuesz : readqueue -> positive

  type writequeue = { wqueue : writequeueentity list; wqueuesz : positive }

  val wqueue : writequeue -> writequeueentity list

  val wqueuesz : writequeue -> positive

  type assemblycache = { acenvs : Environment.EnvironmentReadable.coq_E list;
                         acsize : nat;
                         acwriteenv : Environment.EnvironmentWritable.coq_E;
                         acconfig : Configuration.configuration;
                         acwriteq : writequeue; acreadq : readqueue;
                         acfbstore : Store.FBlockListStore.coq_R;
                         ackstore : Store.KeyListStore.coq_R;
                         acfistore : Store.FileinformationStore.coq_R }

  val acenvs : assemblycache -> Environment.EnvironmentReadable.coq_E list

  val acsize : assemblycache -> nat

  val acwriteenv : assemblycache -> Environment.EnvironmentWritable.coq_E

  val acconfig : assemblycache -> Configuration.configuration

  val acwriteq : assemblycache -> writequeue

  val acreadq : assemblycache -> readqueue

  val acfbstore : assemblycache -> Store.FBlockListStore.coq_R

  val ackstore : assemblycache -> Store.KeyListStore.coq_R

  val acfistore : assemblycache -> Store.FileinformationStore.coq_R

  val prepare_assemblycache :
    Configuration.configuration -> positive -> assemblycache

  val enqueue_write_request :
    assemblycache -> writequeueentity -> bool * assemblycache

  val enqueue_read_request :
    assemblycache -> readqueueentity -> bool * assemblycache

  val try_restore_assembly :
    assemblycache -> Assembly.aid_t -> Environment.EnvironmentReadable.coq_E
    option

  val set_envs :
    assemblycache -> Environment.EnvironmentReadable.coq_E list ->
    assemblycache

  val add_fileinformation :
    assemblycache -> Filesupport.fileinformation -> assemblycache

  val add_fileblockinformation :
    assemblycache -> string -> Assembly.blockinformation -> assemblycache

  val ensure_assembly :
    assemblycache -> Assembly.aid_t ->
    (Environment.EnvironmentReadable.coq_E * assemblycache) option

  val run_read_requests :
    assemblycache -> readqueueentity list -> readqueueresult list ->
    readqueueresult list * assemblycache

  val run_write_requests :
    assemblycache -> writequeueentity list -> assemblycache

  val iterate_read_queue :
    assemblycache -> readqueueresult list * assemblycache

  val iterate_write_queue : assemblycache -> assemblycache

  val flush : assemblycache -> assemblycache

  val close : assemblycache -> assemblycache

  val add_key :
    assemblycache -> Assembly.aid_t -> Assembly.keyinformation ->
    assemblycache
 end

module Processor :
 sig
  type processor = { config : Configuration.configuration;
                     cache : AssemblyCache.assemblycache }

  val config : processor -> Configuration.configuration

  val cache : processor -> AssemblyCache.assemblycache

  val cache_sz : positive

  val prepare_processor : Configuration.configuration -> processor

  val update_cache : processor -> AssemblyCache.assemblycache -> processor

  val backup_block : processor -> AssemblyCache.writequeueentity -> processor

  val request_read :
    processor -> AssemblyCache.readqueueentity ->
    AssemblyCache.readqueueresult list * processor

  val run_write_requests : processor -> processor

  val run_read_requests :
    processor -> AssemblyCache.readqueueresult list * processor

  val close : processor -> processor

  val block_sz : n

  val rec_file_backup_inner :
    Assembly.blockinformation list -> processor -> string -> Cstdio.fptr ->
    processor option

  val open_file_backup :
    processor -> Filesupport.fileinformation -> Assembly.blockinformation
    list -> processor option

  val internal_restore_to :
    processor -> Cstdio.fptr -> AssemblyCache.readqueueresult list -> n

  val restore_block_to :
    processor -> Cstdio.fptr -> AssemblyCache.assemblycache ->
    Assembly.blockinformation -> n * AssemblyCache.assemblycache

  val restore_file_to :
    processor -> Cstdio.fptr -> Assembly.blockinformation list ->
    n * AssemblyCache.assemblycache

  val prepare_blocks' :
    nat -> positive -> n -> n -> Assembly.blockinformation list ->
    Assembly.blockinformation list

  val zip_blocks :
    Assembly.blockinformation list -> Assembly.blockinformation list ->
    Assembly.blockinformation list -> Assembly.blockinformation list

  val prepare_blocks :
    Assembly.blockinformation list -> n -> Assembly.blockinformation list

  val file_backup :
    processor -> (string -> string option) -> (string ->
    Assembly.blockinformation list) -> Filesystem.path -> processor

  val file_restore :
    processor -> Filesystem.path -> Filesystem.path ->
    Assembly.blockinformation list -> n * processor

  val list_directory_entries :
    Filesystem.path -> Filesystem.path list * Filesystem.path list
 end

module Version :
 sig
  val major : string

  val minor : string

  val build : string

  val version : string
 end
