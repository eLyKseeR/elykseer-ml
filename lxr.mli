
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

val const : 'a1 -> 'a2 -> 'a1

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

type 't settable =
  't -> 't
  (* singleton inductive, whose constructor was Build_Settable *)

type ('r, 't) setter = ('t -> 't) -> 'r -> 'r

val set : ('a1 -> 'a2) -> ('a1, 'a2) setter -> ('a2 -> 'a2) -> 'a1 -> 'a1

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

module Buffer :
 sig
  type coq_EncryptionState =
  | Plain
  | Encrypted

  val coq_EncryptionState_rect : 'a1 -> 'a1 -> coq_EncryptionState -> 'a1

  val coq_EncryptionState_rec : 'a1 -> 'a1 -> coq_EncryptionState -> 'a1

  type cstdio_buffer = Mlcpp_cstdio.Cstdio.File.Buffer.ta

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

module Configuration :
 sig
  type configuration = { config_nchunks : Nchunks.t; path_chunks : string;
                         path_db : string; my_id : string }

  val config_nchunks : configuration -> Nchunks.t

  val path_chunks : configuration -> string

  val path_db : configuration -> string

  val my_id : configuration -> string
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

  type assemblyinformation = { nchunks : Nchunks.Private.t; aid : aid_t;
                               apos : n }

  val nchunks : assemblyinformation -> Nchunks.Private.t

  val aid : assemblyinformation -> aid_t

  val apos : assemblyinformation -> n

  val etaX : assemblyinformation settable

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

  val id_assembly_full_ainfo_from_writable :
    assemblyinformation -> assemblyinformation

  val id_assembly_full_buffer_from_writable :
    AssemblyPlainWritable.coq_B -> AssemblyPlainFull.coq_B

  val finish :
    assemblyinformation -> AssemblyPlainWritable.coq_B ->
    assemblyinformation * AssemblyPlainFull.coq_B

  val assembly_add_content :
    Buffer.BufferPlain.buffer_t -> n -> n -> AssemblyPlainWritable.coq_B -> n

  val backup :
    assemblyinformation -> AssemblyPlainWritable.coq_B -> n ->
    Buffer.BufferPlain.buffer_t -> assemblyinformation * blockinformation

  val id_buffer_t_from_full :
    AssemblyPlainFull.coq_B -> Buffer.BufferPlain.buffer_t

  val id_assembly_enc_buffer_t_from_buf :
    Buffer.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B

  val encrypt :
    assemblyinformation -> AssemblyPlainFull.coq_B -> keyinformation ->
    (assemblyinformation * AssemblyEncrypted.coq_B) option

  val assembly_get_content :
    AssemblyPlainFull.coq_B -> n -> n -> Buffer.BufferPlain.buffer_t -> n

  val restore :
    AssemblyPlainFull.coq_B -> blockinformation ->
    Buffer.BufferPlain.buffer_t option

  val id_buffer_t_from_enc :
    AssemblyEncrypted.coq_B -> Buffer.BufferEncrypted.buffer_t

  val id_assembly_plain_buffer_t_from_buf :
    Buffer.BufferPlain.buffer_t -> AssemblyPlainFull.coq_B

  val decrypt :
    assemblyinformation -> AssemblyEncrypted.coq_B -> keyinformation ->
    (assemblyinformation * AssemblyPlainFull.coq_B) option

  val chunk_identifier :
    Configuration.configuration -> aid_t -> positive -> string

  val chunk_identifier_path :
    Configuration.configuration -> aid_t -> positive -> string

  val ext_load_chunk_from_path :
    string -> Buffer.BufferEncrypted.buffer_t option

  val id_enc_from_buffer_t :
    Buffer.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B

  val recall :
    Configuration.configuration -> assemblyinformation ->
    (assemblyinformation * AssemblyEncrypted.coq_B) option

  val ext_store_chunk_to_path :
    string -> n -> n -> Buffer.BufferEncrypted.buffer_t -> n

  val extract :
    Configuration.configuration -> assemblyinformation ->
    AssemblyEncrypted.coq_B -> n
 end

module Store :
 sig
  type 'kVs store = { config : Configuration.configuration; entries : 'kVs }

  val config : 'a1 store -> Configuration.configuration

  val entries : 'a1 store -> 'a1

  val rec_find : string -> (string * 'a1) list -> 'a1 option

  module type STORE =
   sig
    type coq_K

    type coq_V

    type coq_KVs

    type coq_R

    val init : Configuration.configuration -> coq_R

    val add : coq_K -> coq_V -> coq_R -> coq_R

    val find : coq_K -> coq_R -> coq_V option
   end

  module KeyListStore :
   sig
    type coq_K = string

    type coq_V = Assembly.keyinformation

    type coq_KVs = (coq_K * coq_V) list

    type coq_R = coq_KVs store

    val init : Configuration.configuration -> coq_R

    val add : coq_K -> coq_V -> coq_R -> coq_R

    val find : coq_K -> coq_R -> coq_V option
   end

  module FBlockListStore :
   sig
    type coq_K = Assembly.aid_t

    type coq_V = Assembly.blockinformation

    type coq_KVs = (coq_K * coq_V) list

    type coq_R = coq_KVs store

    val init : Configuration.configuration -> coq_R

    val add : coq_K -> coq_V -> coq_R -> coq_R

    val find : coq_K -> coq_R -> coq_V option
   end
 end

module Environment :
 sig
  type 'aB environment = { cur_assembly : Assembly.assemblyinformation;
                           cur_buffer : 'aB;
                           config : Configuration.configuration;
                           fblocks : Store.FBlockListStore.coq_R;
                           keys : Store.KeyListStore.coq_R }

  val cur_assembly : 'a1 environment -> Assembly.assemblyinformation

  val cur_buffer : 'a1 environment -> 'a1

  val config : 'a1 environment -> Configuration.configuration

  val fblocks : 'a1 environment -> Store.FBlockListStore.coq_R

  val keys : 'a1 environment -> Store.KeyListStore.coq_R

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

    val env_add_file_block :
      string -> coq_AB environment -> Assembly.blockinformation -> coq_AB
      environment

    val env_add_aid_key :
      Assembly.aid_t -> coq_AB environment -> Assembly.keyinformation ->
      coq_AB environment

    val key_for_aid :
      coq_AB environment -> Assembly.aid_t -> Assembly.keyinformation option

    val finalise_assembly : coq_AB environment -> coq_AB environment

    val finalise_and_recreate_assembly :
      coq_AB environment -> coq_AB environment

    val backup :
      coq_AB environment -> string -> n -> Buffer.BufferPlain.buffer_t ->
      coq_AB environment
   end

  module EnvironmentReadable :
   sig
    type coq_AB = Assembly.AssemblyPlainFull.coq_B

    type coq_E = coq_AB environment

    val initial_environment : Configuration.configuration -> coq_E

    val env_add_aid_key :
      Assembly.aid_t -> coq_AB environment -> Assembly.keyinformation ->
      coq_AB environment

    val key_for_aid :
      coq_AB environment -> Assembly.aid_t -> Assembly.keyinformation option

    val restore_assembly :
      coq_AB environment -> Assembly.aid_t -> coq_AB environment option
   end
 end

module AssemblyCache :
 sig
  type readqueueentity = { qaid : Assembly.aid_t; qapos : n; qrlen : n }

  val qaid : readqueueentity -> Assembly.aid_t

  val qapos : readqueueentity -> n

  val qrlen : readqueueentity -> n

  type readqueueresult = { readrequest : readqueueentity;
                           rresult : Buffer.BufferPlain.buffer_t }

  val readrequest : readqueueresult -> readqueueentity

  val rresult : readqueueresult -> Buffer.BufferPlain.buffer_t

  type writequeueentity = { qfhash : string; qfpos : n;
                            qbuffer : Buffer.BufferPlain.buffer_t }

  val qfhash : writequeueentity -> string

  val qfpos : writequeueentity -> n

  val qbuffer : writequeueentity -> Buffer.BufferPlain.buffer_t

  type writequeueresult = { writerequest : writequeueentity;
                            wresult : readqueueentity }

  val writerequest : writequeueresult -> writequeueentity

  val wresult : writequeueresult -> readqueueentity

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
                         acwriteq : writequeue; acreadq : readqueue }

  val acenvs : assemblycache -> Environment.EnvironmentReadable.coq_E list

  val acsize : assemblycache -> nat

  val acwriteenv : assemblycache -> Environment.EnvironmentWritable.coq_E

  val acconfig : assemblycache -> Configuration.configuration

  val acwriteq : assemblycache -> writequeue

  val acreadq : assemblycache -> readqueue

  val prepare_assemblycache :
    Configuration.configuration -> positive -> assemblycache

  val enqueue_write_request :
    assemblycache -> writequeueentity -> bool * assemblycache

  val enqueue_read_request :
    assemblycache -> readqueueentity -> bool * assemblycache

  val try_restore_assembly :
    Configuration.configuration -> Assembly.aid_t ->
    Environment.EnvironmentReadable.coq_E option

  val set_envs :
    assemblycache -> Environment.EnvironmentReadable.coq_E list ->
    assemblycache

  val ensure_assembly :
    assemblycache -> Assembly.aid_t ->
    (Environment.EnvironmentReadable.coq_E * assemblycache) option

  val run_read_requests :
    assemblycache -> readqueueentity list -> readqueueresult list ->
    readqueueresult list * assemblycache

  val run_write_requests :
    assemblycache -> writequeueentity list -> writequeueresult list ->
    writequeueresult list * assemblycache

  val iterate_read_queue :
    assemblycache -> readqueueresult list * assemblycache

  val iterate_write_queue :
    assemblycache -> writequeueresult list * assemblycache

  val flush : assemblycache -> assemblycache

  val close : assemblycache -> assemblycache
 end

module Filesupport :
 sig
  type filename = string

  type fileinformation = { fname : filename; fsize : n; fowner : string;
                           fpermissions : n; fmodified : string;
                           fchecksum : string }

  val fname : fileinformation -> filename

  val fsize : fileinformation -> n

  val fowner : fileinformation -> string

  val fpermissions : fileinformation -> n

  val fmodified : fileinformation -> string

  val fchecksum : fileinformation -> string

  val get_file_information : filename -> fileinformation
 end

module BackupPlanner :
 sig
  type fileblock = { fbanum : positive; fbfpos : n; fbsz : n }

  val fbanum : fileblock -> positive

  val fbfpos : fileblock -> n

  val fbsz : fileblock -> n

  type fileblockinformation = { fbifi : Filesupport.fileinformation;
                                fbifblocks : fileblock list }

  val fbifi : fileblockinformation -> Filesupport.fileinformation

  val fbifblocks : fileblockinformation -> fileblock list

  val aminsz : n

  val prepare_blocks :
    positive -> n -> nat -> positive -> n -> fileblock list -> n -> n ->
    fileblock list * (positive * n)

  val max_block_size : n

  val analyse_file :
    positive -> n -> positive -> string ->
    (positive * n) * fileblockinformation
 end

module Version :
 sig
  val major : string

  val minor : string

  val build : string

  val version : string
 end
