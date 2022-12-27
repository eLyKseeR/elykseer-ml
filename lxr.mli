
type nat =
| O
| S of nat

val fst : ('a1 * 'a2) -> 'a1

val app : 'a1 list -> 'a1 list -> 'a1 list

type comparison =
| Eq
| Lt
| Gt

val add : nat -> nat -> nat

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

val rev : 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1

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
 end

module Configuration :
 sig
  type configuration = { config_nchunks : Nchunks.t; path_chunks : string;
                         path_db : string; my_id : n }

  val config_nchunks : configuration -> Nchunks.t

  val path_chunks : configuration -> string

  val path_db : configuration -> string

  val my_id : configuration -> n
 end

module Utilities :
 sig
  val make_list : positive -> positive list

  val rnd : n -> n

  val rnd256 : n -> string
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

  type keyinformation = { ivec : string; pkey : string; localid : n;
                          localnchunks : positive }

  val ivec : keyinformation -> string

  val pkey : keyinformation -> string

  val localid : keyinformation -> n

  val localnchunks : keyinformation -> positive

  module type ASS =
   sig
    type coq_H = assemblyinformation

    type coq_B

    val create : Configuration.configuration -> coq_H * coq_B

    val buffer_len : coq_B -> n

    val calc_checksum : coq_B -> string
   end

  module AssemblyPlainWritable :
   ASS

  module AssemblyEncrypted :
   ASS

  module AssemblyPlainFull :
   ASS

  val id_buffer_t_from_enc :
    AssemblyEncrypted.coq_B -> Buffer.BufferEncrypted.buffer_t

  val id_enc_from_buffer_t :
    Buffer.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B

  val id_assembly_plain_buffer_t_from_buf :
    Buffer.BufferPlain.buffer_t -> AssemblyPlainWritable.coq_B

  val decrypt :
    AssemblyEncrypted.coq_H -> AssemblyEncrypted.coq_B -> keyinformation ->
    (AssemblyPlainWritable.coq_H * AssemblyPlainWritable.coq_B) option

  val chunk_identifier :
    Configuration.configuration -> aid_t -> positive -> string

  val chunk_identifier_path :
    Configuration.configuration -> aid_t -> positive -> string

  val ext_load_chunk_from_path :
    string -> Buffer.BufferEncrypted.buffer_t option

  val recall :
    Configuration.configuration -> AssemblyEncrypted.coq_H ->
    (AssemblyEncrypted.coq_H * AssemblyEncrypted.coq_B) option

  val ext_store_chunk_to_path :
    string -> n -> n -> Buffer.BufferEncrypted.buffer_t -> n

  val extract :
    Configuration.configuration -> AssemblyEncrypted.coq_H ->
    AssemblyEncrypted.coq_B -> n

  val id_buffer_t_from_full :
    AssemblyPlainFull.coq_B -> Buffer.BufferPlain.buffer_t

  val id_buffer_t_from_writable :
    AssemblyPlainWritable.coq_B -> Buffer.BufferPlain.buffer_t

  val id_assembly_enc_buffer_t_from_buf :
    Buffer.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B

  val id_assembly_full_buffer_from_writable :
    AssemblyPlainWritable.coq_B -> AssemblyPlainFull.coq_B

  val finish :
    AssemblyPlainWritable.coq_H -> AssemblyPlainWritable.coq_B ->
    AssemblyPlainFull.coq_H * AssemblyPlainFull.coq_B

  val encrypt :
    AssemblyPlainFull.coq_H -> AssemblyPlainFull.coq_B -> keyinformation ->
    (AssemblyEncrypted.coq_H * AssemblyEncrypted.coq_B) option

  type blockinformation = { blockid : positive; bchecksum : string;
                            blocksize : n; filepos : n; blockaid : string;
                            blockapos : n }

  val blockid : blockinformation -> positive

  val bchecksum : blockinformation -> string

  val blocksize : blockinformation -> n

  val filepos : blockinformation -> n

  val blockaid : blockinformation -> string

  val blockapos : blockinformation -> n

  val assembly_add_content :
    Buffer.BufferPlain.buffer_t -> n -> n -> AssemblyPlainWritable.coq_B -> n

  val backup :
    AssemblyPlainWritable.coq_H -> AssemblyPlainWritable.coq_B -> n ->
    Buffer.BufferPlain.buffer_t ->
    AssemblyPlainWritable.coq_H * blockinformation

  val assembly_get_content :
    AssemblyPlainFull.coq_B -> n -> n -> Buffer.BufferPlain.buffer_t -> n

  val restore :
    AssemblyPlainFull.coq_B -> blockinformation ->
    Buffer.BufferPlain.buffer_t option
 end

module Filetypes :
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
 end

module Environment :
 sig
  type environment = { cur_assembly : Assembly.AssemblyPlainWritable.coq_H;
                       cur_buffer : Assembly.AssemblyPlainWritable.coq_B;
                       config : Configuration.configuration;
                       fblocks : (string * Assembly.blockinformation) list;
                       keys : (string * Assembly.keyinformation) list }

  val cur_assembly : environment -> Assembly.AssemblyPlainWritable.coq_H

  val cur_buffer : environment -> Assembly.AssemblyPlainWritable.coq_B

  val config : environment -> Configuration.configuration

  val fblocks : environment -> (string * Assembly.blockinformation) list

  val keys : environment -> (string * Assembly.keyinformation) list

  val initial_environment : Configuration.configuration -> environment

  val recreate_assembly : environment -> environment

  val env_add_file_block :
    string -> environment -> Assembly.blockinformation -> environment

  val env_add_aid_key :
    string -> environment -> Assembly.keyinformation -> environment

  val backup :
    environment -> string -> n -> Buffer.BufferPlain.buffer_t -> environment
 end

module Filesupport :
 sig
  val get_file_information : Filetypes.filename -> Filetypes.fileinformation
 end

module BackupPlanner :
 sig
  type fileblock = { fbanum : positive; fbfpos : n; fbsz : n }

  val fbanum : fileblock -> positive

  val fbfpos : fileblock -> n

  val fbsz : fileblock -> n

  type fileblockinformation = { fbifi : Filetypes.fileinformation;
                                fbifblocks : fileblock list }

  val fbifi : fileblockinformation -> Filetypes.fileinformation

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
