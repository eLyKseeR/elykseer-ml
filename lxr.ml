
(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

type nat =
| O
| S of nat

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val length : 'a1 list -> nat **)

let rec length = function
| [] -> O
| _ :: l' -> S (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

type comparison =
| Eq
| Lt
| Gt

module Coq__1 = struct
 (** val add : nat -> nat -> nat **)

 let rec add n0 m =
   match n0 with
   | O -> m
   | S p -> S (add p m)
end
include Coq__1

(** val eqb : nat -> nat -> bool **)

let rec eqb n0 m =
  match n0 with
  | O -> (match m with
          | O -> true
          | S _ -> false)
  | S n' -> (match m with
             | O -> false
             | S m' -> eqb n' m')

(** val leb : nat -> nat -> bool **)

let rec leb n0 m =
  match n0 with
  | O -> true
  | S n' -> (match m with
             | O -> false
             | S m' -> leb n' m')

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

(** val const : 'a1 -> 'a2 -> 'a1 **)

let const a _ =
  a

module Pos =
 struct
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg
 end

module Coq_Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val add : positive -> positive -> positive **)

  let rec add x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XO p ->
      (match y with
       | XI q -> XI (add p q)
       | XO q -> XO (add p q)
       | XH -> XI p)
    | XH -> (match y with
             | XI q -> XO (succ q)
             | XO q -> XI q
             | XH -> XO XH)

  (** val add_carry : positive -> positive -> positive **)

  and add_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XI (add_carry p q)
       | XO q -> XO (add_carry p q)
       | XH -> XI (succ p))
    | XO p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XH ->
      (match y with
       | XI q -> XI (succ q)
       | XO q -> XO (succ q)
       | XH -> XI XH)

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  (** val succ_double_mask : mask -> mask **)

  let succ_double_mask = function
  | IsNul -> IsPos XH
  | IsPos p -> IsPos (XI p)
  | IsNeg -> IsNeg

  (** val double_mask : mask -> mask **)

  let double_mask = function
  | IsPos p -> IsPos (XO p)
  | x0 -> x0

  (** val double_pred_mask : positive -> mask **)

  let double_pred_mask = function
  | XI p -> IsPos (XO (XO p))
  | XO p -> IsPos (XO (pred_double p))
  | XH -> IsNul

  (** val sub_mask : positive -> positive -> mask **)

  let rec sub_mask x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> double_mask (sub_mask p q)
       | XO q -> succ_double_mask (sub_mask p q)
       | XH -> IsPos (XO p))
    | XO p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XH -> (match y with
             | XH -> IsNul
             | _ -> IsNeg)

  (** val sub_mask_carry : positive -> positive -> mask **)

  and sub_mask_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XO p ->
      (match y with
       | XI q -> double_mask (sub_mask_carry p q)
       | XO q -> succ_double_mask (sub_mask_carry p q)
       | XH -> double_pred_mask p)
    | XH -> IsNeg

  (** val mul : positive -> positive -> positive **)

  let rec mul x y =
    match x with
    | XI p -> add y (XO (mul p y))
    | XO p -> XO (mul p y)
    | XH -> y

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> compare_cont r p q
       | XO q -> compare_cont Gt p q
       | XH -> Gt)
    | XO p ->
      (match y with
       | XI q -> compare_cont Lt p q
       | XO q -> compare_cont r p q
       | XH -> Gt)
    | XH -> (match y with
             | XH -> r
             | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq

  (** val min : positive -> positive -> positive **)

  let min p p' =
    match compare p p' with
    | Gt -> p'
    | _ -> p

  (** val max : positive -> positive -> positive **)

  let max p p' =
    match compare p p' with
    | Gt -> p
    | _ -> p'

  (** val eqb : positive -> positive -> bool **)

  let rec eqb p q =
    match p with
    | XI p0 -> (match q with
                | XI q0 -> eqb p0 q0
                | _ -> false)
    | XO p0 -> (match q with
                | XO q0 -> eqb p0 q0
                | _ -> false)
    | XH -> (match q with
             | XH -> true
             | _ -> false)

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)

  let rec iter_op op p a =
    match p with
    | XI p0 -> op a (iter_op op p0 (op a a))
    | XO p0 -> iter_op op p0 (op a a)
    | XH -> a

  (** val to_nat : positive -> nat **)

  let to_nat x =
    iter_op Coq__1.add x (S O)

  (** val of_nat : nat -> positive **)

  let rec of_nat = function
  | O -> XH
  | S x -> (match x with
            | O -> XH
            | S _ -> succ (of_nat x))

  (** val of_succ_nat : nat -> positive **)

  let rec of_succ_nat = function
  | O -> XH
  | S x -> succ (of_succ_nat x)
 end

module N =
 struct
  (** val succ_double : n -> n **)

  let succ_double = function
  | N0 -> Npos XH
  | Npos p -> Npos (XI p)

  (** val double : n -> n **)

  let double = function
  | N0 -> N0
  | Npos p -> Npos (XO p)

  (** val add : n -> n -> n **)

  let add n0 m =
    match n0 with
    | N0 -> m
    | Npos p -> (match m with
                 | N0 -> n0
                 | Npos q -> Npos (Coq_Pos.add p q))

  (** val sub : n -> n -> n **)

  let sub n0 m =
    match n0 with
    | N0 -> N0
    | Npos n' ->
      (match m with
       | N0 -> n0
       | Npos m' ->
         (match Coq_Pos.sub_mask n' m' with
          | Coq_Pos.IsPos p -> Npos p
          | _ -> N0))

  (** val mul : n -> n -> n **)

  let mul n0 m =
    match n0 with
    | N0 -> N0
    | Npos p -> (match m with
                 | N0 -> N0
                 | Npos q -> Npos (Coq_Pos.mul p q))

  (** val compare : n -> n -> comparison **)

  let compare n0 m =
    match n0 with
    | N0 -> (match m with
             | N0 -> Eq
             | Npos _ -> Lt)
    | Npos n' -> (match m with
                  | N0 -> Gt
                  | Npos m' -> Coq_Pos.compare n' m')

  (** val eqb : n -> n -> bool **)

  let eqb n0 m =
    match n0 with
    | N0 -> (match m with
             | N0 -> true
             | Npos _ -> false)
    | Npos p -> (match m with
                 | N0 -> false
                 | Npos q -> Coq_Pos.eqb p q)

  (** val leb : n -> n -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val ltb : n -> n -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val pos_div_eucl : positive -> n -> n * n **)

  let rec pos_div_eucl a b =
    match a with
    | XI a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = succ_double r in
      if leb b r' then ((succ_double q), (sub r' b)) else ((double q), r')
    | XO a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = double r in
      if leb b r' then ((succ_double q), (sub r' b)) else ((double q), r')
    | XH ->
      (match b with
       | N0 -> (N0, (Npos XH))
       | Npos p -> (match p with
                    | XH -> ((Npos XH), N0)
                    | _ -> (N0, (Npos XH))))

  (** val div_eucl : n -> n -> n * n **)

  let div_eucl a b =
    match a with
    | N0 -> (N0, N0)
    | Npos na -> (match b with
                  | N0 -> (N0, a)
                  | Npos _ -> pos_div_eucl na b)

  (** val div : n -> n -> n **)

  let div a b =
    fst (div_eucl a b)

  (** val to_nat : n -> nat **)

  let to_nat = function
  | N0 -> O
  | Npos p -> Coq_Pos.to_nat p

  (** val of_nat : nat -> n **)

  let of_nat = function
  | O -> N0
  | S n' -> Npos (Coq_Pos.of_succ_nat n')
 end

(** val removelast : 'a1 list -> 'a1 list **)

let rec removelast = function
| [] -> []
| a :: l0 -> (match l0 with
              | [] -> []
              | _ :: _ -> a :: (removelast l0))

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| [] -> []
| x :: l' -> app (rev l') (x :: [])

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t0 -> (f a) :: (map f t0)

(** val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let rec fold_left f l a0 =
  match l with
  | [] -> a0
  | b :: t0 -> fold_left f t0 (f a0 b)

(** val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list **)

let rec filter f = function
| [] -> []
| x :: l0 -> if f x then x :: (filter f l0) else filter f l0

(** val seq : nat -> nat -> nat list **)

let rec seq start = function
| O -> []
| S len0 -> start :: (seq (S start) len0)

type 't settable =
  't -> 't
  (* singleton inductive, whose constructor was Build_Settable *)

type ('r, 't) setter = ('t -> 't) -> 'r -> 'r

(** val set :
    ('a1 -> 'a2) -> ('a1, 'a2) setter -> ('a2 -> 'a2) -> 'a1 -> 'a1 **)

let set _ setter0 =
  setter0

module Conversion =
 struct
  (** val pos2N : positive -> n **)

  let pos2N p =
    Npos p

  (** val nat2N : nat -> n **)

  let nat2N =
    N.of_nat

  (** val i2p : int -> positive **)

  let i2p =   
    let rec i2p = function 
       1 -> XH 
     | n -> let n' = i2p (n/2) in if (n mod 2)=0 then XO n' else XI n'
     in i2p
   

  (** val p2i : positive -> int **)

  let p2i = 
    let rec p2i = function 
       XH -> 1
     | XO p -> 2*(p2i p)
     | XI p -> 2*(p2i p)+1
     in p2i 
   

  (** val i2z : int -> z **)

  let i2z = 
    function 
      0 -> Z0
    | n -> if n < 0 then Zneg (i2p (-n)) else Zpos (i2p n)
   

  (** val z2i : z -> int **)

  let z2i = 
    function
      Z0 -> 0 
    | Zpos p -> p2i p
    | Zneg p -> -(p2i p)
   

  (** val i2n : int -> n **)

  let i2n = 
    function 
      0 -> N0
    | n -> Npos (i2p n)
   

  (** val n2i : n -> int **)

  let n2i = 
    function
      N0 -> 0 
    | Npos p -> p2i p
   
 end

module Nchunks =
 struct
  (** val max_n : positive **)

  let max_n =
    XO (XO (XO (XO (XO (XO (XO (XO XH)))))))

  (** val min_n : positive **)

  let min_n =
    XO (XO (XO (XO XH)))

  module Private =
   struct
    type t = positive

    (** val from_positive : positive -> t **)

    let from_positive n0 =
      Coq_Pos.min max_n (Coq_Pos.max n0 min_n)

    (** val from_int : int -> t **)

    let from_int i =
      from_positive (Conversion.i2p i)

    (** val to_positive : t -> positive **)

    let to_positive x =
      x

    (** val to_N : t -> n **)

    let to_N =
      Conversion.pos2N
   end

  type t = Private.t

  (** val from_positive : positive -> Private.t **)

  let from_positive =
    Private.from_positive

  (** val from_int : int -> Private.t **)

  let from_int =
    Private.from_int

  (** val to_positive : Private.t -> positive **)

  let to_positive =
    Private.to_positive

  (** val to_N : Private.t -> n **)

  let to_N =
    Private.to_N
 end

module Cstdio =
 struct
  type coq_EncryptionState =
  | Plain
  | Encrypted

  (** val coq_EncryptionState_rect :
      'a1 -> 'a1 -> coq_EncryptionState -> 'a1 **)

  let coq_EncryptionState_rect f f0 = function
  | Plain -> f
  | Encrypted -> f0

  (** val coq_EncryptionState_rec :
      'a1 -> 'a1 -> coq_EncryptionState -> 'a1 **)

  let coq_EncryptionState_rec f f0 = function
  | Plain -> f
  | Encrypted -> f0

  type cstdio_buffer = Mlcpp_cstdio.Cstdio.File.Buffer.ta

  type mode = string

  (** val read_mode : mode **)

  let read_mode =
    "rb"

  (** val write_mode : mode **)

  let write_mode =
    "wb"

  (** val write_new_mode : mode **)

  let write_new_mode =
    "wx"

  (** val append_mode : mode **)

  let append_mode =
    "ab"

  type fptr = Mlcpp_cstdio.Cstdio.File.file

  (** val fopen : string -> mode -> fptr option **)

  let fopen = fun fname mode ->
      match Mlcpp_cstdio.Cstdio.File.fopen fname mode with
      | Ok fptr -> Some fptr
      | Error (errno, errstr) -> Printf.printf "fopen '%s' error: %d/%s\n" fname errno errstr; None
   

  (** val fclose : fptr -> unit option **)

  let fclose = fun fptr ->
      match Mlcpp_cstdio.Cstdio.File.fclose fptr with
      | Ok () -> Some ()
      | Error (errno, errstr) -> Printf.printf "fclose error: %d/%s\n" errno errstr; None
   

  (** val fflush : fptr -> fptr option **)

  let fflush = fun fptr ->
      match Mlcpp_cstdio.Cstdio.File.fflush fptr with
      | Ok () -> Some fptr
      | Error (errno, errstr) -> Printf.printf "fflush error: %d/%s\n" errno errstr; None
   

  (** val fread : fptr -> n -> (n * cstdio_buffer) option **)

  let fread = fun fptr sz ->
      let b = Mlcpp_cstdio.Cstdio.File.Buffer.create (Conversion.n2i sz) in
      match Mlcpp_cstdio.Cstdio.File.fread b (Conversion.n2i sz) fptr with
      | Ok nread -> Some (Conversion.i2n nread, b)
      | Error (errno, errstr) -> Printf.printf "fread error: %d/%s\n" errno errstr; None
   

  (** val fwrite : fptr -> n -> cstdio_buffer -> n option **)

  let fwrite = fun fptr sz b ->
      match Mlcpp_cstdio.Cstdio.File.fwrite b (Conversion.n2i sz) fptr with
      | Ok nwritten -> Some (Conversion.i2n nwritten)
      | Error (errno, errstr) -> Printf.printf "fwrite error: %d/%s\n" errno errstr; None
   

  (** val ftell : fptr -> n option **)

  let ftell = fun fptr ->
      match Mlcpp_cstdio.Cstdio.File.ftell fptr with
      | Ok pos -> Some (Conversion.i2n pos)
      | Error (errno, errstr) -> Printf.printf "ftell error: %d/%s\n" errno errstr; None
   

  (** val fseek : fptr -> n -> fptr option **)

  let fseek = fun fptr pos ->
      match Mlcpp_cstdio.Cstdio.File.fseek fptr (Conversion.n2i pos) with
      | Ok () -> Some fptr
      | Error (errno, errstr) -> Printf.printf "fseek error: %d/%s\n" errno errstr; None
   

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

  module BufferEncrypted =
   struct
    type buffer_t = Mlcpp_cstdio.Cstdio.File.Buffer.ta

    (** val buffer_create : n -> buffer_t **)

    let buffer_create = fun n -> Mlcpp_cstdio.Cstdio.File.Buffer.create (Conversion.n2i n)

    (** val buffer_len : buffer_t -> n **)

    let buffer_len = fun b -> Conversion.i2n (Mlcpp_cstdio.Cstdio.File.Buffer.size b)

    (** val calc_checksum : buffer_t -> string **)

    let calc_checksum = fun b -> Elykseer_crypto.Sha256.buffer b

    (** val copy_sz_pos : buffer_t -> n -> n -> buffer_t -> n -> n **)

    let copy_sz_pos = 
    fun bsrc npos1 nsz btgt npos2 -> Conversion.i2n @@
      Mlcpp_cstdio.Cstdio.File.Buffer.copy_sz_pos bsrc ~pos1:(Conversion.n2i npos1) ~sz:(Conversion.n2i nsz) btgt ~pos2:(Conversion.n2i npos2)
   

    (** val from_buffer : cstdio_buffer -> buffer_t **)

    let from_buffer = fun b -> Helper.cpp_buffer_id b

    (** val to_buffer : buffer_t -> cstdio_buffer **)

    let to_buffer = fun b -> Helper.cpp_buffer_id b

    (** val state : coq_EncryptionState **)

    let state =
      Encrypted
   end

  module BufferPlain =
   struct
    type buffer_t = Mlcpp_cstdio.Cstdio.File.Buffer.ta

    (** val buffer_create : n -> buffer_t **)

    let buffer_create = fun n -> Mlcpp_cstdio.Cstdio.File.Buffer.create (Conversion.n2i n)

    (** val buffer_len : buffer_t -> n **)

    let buffer_len = fun b -> Conversion.i2n (Mlcpp_cstdio.Cstdio.File.Buffer.size b)

    (** val calc_checksum : buffer_t -> string **)

    let calc_checksum = fun b -> Elykseer_crypto.Sha256.buffer b

    (** val copy_sz_pos : buffer_t -> n -> n -> buffer_t -> n -> n **)

    let copy_sz_pos = 
    fun bsrc npos1 nsz btgt npos2 -> Conversion.i2n @@
      Mlcpp_cstdio.Cstdio.File.Buffer.copy_sz_pos bsrc ~pos1:(Conversion.n2i npos1) ~sz:(Conversion.n2i nsz) btgt ~pos2:(Conversion.n2i npos2)
   

    (** val from_buffer : cstdio_buffer -> buffer_t **)

    let from_buffer = fun b -> Helper.cpp_buffer_id b

    (** val to_buffer : buffer_t -> cstdio_buffer **)

    let to_buffer = fun b -> Helper.cpp_buffer_id b

    (** val state : coq_EncryptionState **)

    let state =
      Plain
   end

  (** val cpp_encrypt_buffer :
      BufferPlain.buffer_t -> string -> string -> BufferEncrypted.buffer_t **)

  let cpp_encrypt_buffer = fun b siv spk -> Elykseer_crypto.Aes256.encrypt (Elykseer_crypto.Key128.from_hex siv) (Elykseer_crypto.Key256.from_hex spk) b

  (** val encrypt :
      BufferPlain.buffer_t -> string -> string -> BufferEncrypted.buffer_t **)

  let encrypt =
    cpp_encrypt_buffer

  (** val cpp_decrypt_buffer :
      BufferEncrypted.buffer_t -> string -> string -> BufferPlain.buffer_t **)

  let cpp_decrypt_buffer = fun b siv spk -> Elykseer_crypto.Aes256.decrypt (Elykseer_crypto.Key128.from_hex siv) (Elykseer_crypto.Key256.from_hex spk) b

  (** val decrypt :
      BufferEncrypted.buffer_t -> string -> string -> BufferPlain.buffer_t **)

  let decrypt =
    cpp_decrypt_buffer

  (** val cpp_ranbuf128 : unit -> cstdio_buffer **)

  let cpp_ranbuf128 = fun () -> Helper.ranbuf128 ()

  (** val ranbuf128 : unit -> BufferPlain.buffer_t **)

  let ranbuf128 _ =
    let rb = cpp_ranbuf128 () in BufferPlain.from_buffer rb
 end

module Configuration =
 struct
  type configuration = { config_nchunks : Nchunks.t; path_chunks : string;
                         path_db : string; my_id : string }

  (** val config_nchunks : configuration -> Nchunks.t **)

  let config_nchunks c =
    c.config_nchunks

  (** val path_chunks : configuration -> string **)

  let path_chunks c =
    c.path_chunks

  (** val path_db : configuration -> string **)

  let path_db c =
    c.path_db

  (** val my_id : configuration -> string **)

  let my_id c =
    c.my_id
 end

module Filesystem =
 struct
  type path = Mlcpp_filesystem.Filesystem.path

  module Path =
   struct
    (** val to_string : path -> string **)

    let to_string = Mlcpp_filesystem.Filesystem.Path.to_string

    (** val from_string : string -> path **)

    let from_string = Mlcpp_filesystem.Filesystem.Path.from_string

    (** val append : path -> path -> path **)

    let append = Mlcpp_filesystem.Filesystem.Path.append

    (** val temp_directory : unit -> path **)

    let temp_directory = Mlcpp_filesystem.Filesystem.Path.temp_directory

    (** val file_exists : path -> bool **)

    let file_exists = Mlcpp_filesystem.Filesystem.Path.exists

    (** val file_size : path -> n **)

    let file_size = fun p -> Conversion.i2n (Mlcpp_filesystem.Filesystem.Path.file_size p)

    (** val filename : path -> path **)

    let filename = Mlcpp_filesystem.Filesystem.Path.filename

    (** val extension : path -> path **)

    let extension = Mlcpp_filesystem.Filesystem.Path.extension

    (** val parent : path -> path **)

    let parent = Mlcpp_filesystem.Filesystem.Path.parent

    (** val root : path -> path **)

    let root = Mlcpp_filesystem.Filesystem.Path.root

    (** val absolute : path -> path option **)

    let absolute = Mlcpp_filesystem.Filesystem.Path.absolute

    (** val relative : path -> path option **)

    let relative = Mlcpp_filesystem.Filesystem.Path.relative

    (** val proximate : path -> path option **)

    let proximate = Mlcpp_filesystem.Filesystem.Path.proximate

    (** val canonical : path -> path option **)

    let canonical = Mlcpp_filesystem.Filesystem.Path.canonical

    (** val weakly_canonical : path -> path option **)

    let weakly_canonical = Mlcpp_filesystem.Filesystem.Path.weakly_canonical

    (** val path_type : path -> string **)

    let path_type = Mlcpp_filesystem.Filesystem.Path.path_type

    (** val is_regular_file : path -> bool **)

    let is_regular_file = Mlcpp_filesystem.Filesystem.Path.is_regular_file

    (** val is_directory : path -> bool **)

    let is_directory = Mlcpp_filesystem.Filesystem.Path.is_directory

    (** val is_fifo : path -> bool **)

    let is_fifo = Mlcpp_filesystem.Filesystem.Path.is_fifo

    (** val is_block_file : path -> bool **)

    let is_block_file = Mlcpp_filesystem.Filesystem.Path.is_block_file

    (** val is_character_file : path -> bool **)

    let is_character_file = Mlcpp_filesystem.Filesystem.Path.is_character_file

    (** val is_socket : path -> bool **)

    let is_socket = Mlcpp_filesystem.Filesystem.Path.is_socket

    (** val is_symlink : path -> bool **)

    let is_symlink = Mlcpp_filesystem.Filesystem.Path.is_symlink

    (** val is_other : path -> bool **)

    let is_other = Mlcpp_filesystem.Filesystem.Path.is_other
   end

  module Permissions =
   struct
    type permissions = Mlcpp_filesystem.Filesystem.Permissions.permissions

    (** val get : path -> permissions option **)

    let get = Mlcpp_filesystem.Filesystem.Permissions.get

    (** val set : path -> permissions -> bool **)

    let set = Mlcpp_filesystem.Filesystem.Permissions.set

    (** val add : path -> permissions -> bool **)

    let add = Mlcpp_filesystem.Filesystem.Permissions.add

    (** val remove : path -> permissions -> bool **)

    let remove = Mlcpp_filesystem.Filesystem.Permissions.remove

    (** val to_string : permissions -> string **)

    let to_string = Mlcpp_filesystem.Filesystem.Permissions.to_string

    (** val to_dec : permissions -> positive **)

    let to_dec = fun p -> Conversion.i2p (Mlcpp_filesystem.Filesystem.Permissions.to_dec p)

    (** val to_oct : permissions -> positive **)

    let to_oct = fun p -> Conversion.i2p (Mlcpp_filesystem.Filesystem.Permissions.to_oct p)

    (** val from_oct : positive -> permissions **)

    let from_oct = fun o -> Mlcpp_filesystem.Filesystem.Permissions.from_oct (Conversion.p2i o)
   end

  (** val get_cwd : unit -> path **)

  let get_cwd = Mlcpp_filesystem.Filesystem.get_cwd

  (** val set_cwd : path -> bool **)

  let set_cwd = Mlcpp_filesystem.Filesystem.set_cwd

  (** val copy : path -> path -> bool **)

  let copy = Mlcpp_filesystem.Filesystem.copy

  (** val copy_file : path -> path -> bool **)

  let copy_file = Mlcpp_filesystem.Filesystem.copy_file

  (** val copy_symlink : path -> path -> bool **)

  let copy_symlink = Mlcpp_filesystem.Filesystem.copy_symlink

  (** val create_directory : path -> bool **)

  let create_directory = Mlcpp_filesystem.Filesystem.create_directory

  (** val create_directories : path -> bool **)

  let create_directories = Mlcpp_filesystem.Filesystem.create_directories

  (** val create_hard_link : path -> path -> bool **)

  let create_hard_link = Mlcpp_filesystem.Filesystem.create_hard_link

  (** val create_symlink : path -> path -> bool **)

  let create_symlink = Mlcpp_filesystem.Filesystem.create_symlink

  (** val create_directory_symlink : path -> path -> bool **)

  let create_directory_symlink = Mlcpp_filesystem.Filesystem.create_directory_symlink

  (** val equivalent : path -> path -> bool **)

  let equivalent = Mlcpp_filesystem.Filesystem.equivalent

  (** val hard_link_count : path -> positive **)

  let hard_link_count = fun p -> Conversion.i2p (Mlcpp_filesystem.Filesystem.hard_link_count p)

  (** val read_symlink : path -> path option **)

  let read_symlink = Mlcpp_filesystem.Filesystem.read_symlink

  (** val remove : path -> bool **)

  let remove = Mlcpp_filesystem.Filesystem.remove

  (** val remove_all : path -> positive **)

  let remove_all = fun p -> Conversion.i2p (Mlcpp_filesystem.Filesystem.remove_all p)

  (** val rename : path -> path -> bool **)

  let rename = Mlcpp_filesystem.Filesystem.rename

  (** val resize_file : path -> n -> bool **)

  let resize_file = fun p sz -> Mlcpp_filesystem.Filesystem.resize_file p (Conversion.n2i sz)

  (** val space : path -> n list **)

  let space = fun p -> List.map (fun i -> Conversion.i2n i) (Mlcpp_filesystem.Filesystem.space p)

  type direntry = Mlcpp_filesystem.Filesystem.direntry

  module Direntry =
   struct
    (** val as_path : direntry -> path **)

    let as_path = Mlcpp_filesystem.Filesystem.Direntry.as_path

    (** val as_string : direntry -> string **)

    let as_string de =
      Path.to_string (as_path de)

    (** val direntry_exists : direntry -> bool **)

    let direntry_exists = Mlcpp_filesystem.Filesystem.Direntry.direntry_exists

    (** val is_regular_file : direntry -> bool **)

    let is_regular_file = Mlcpp_filesystem.Filesystem.Direntry.is_regular_file

    (** val is_block_file : direntry -> bool **)

    let is_block_file = Mlcpp_filesystem.Filesystem.Direntry.is_block_file

    (** val is_character_file : direntry -> bool **)

    let is_character_file = Mlcpp_filesystem.Filesystem.Direntry.is_character_file

    (** val is_directory : direntry -> bool **)

    let is_directory = Mlcpp_filesystem.Filesystem.Direntry.is_directory

    (** val is_fifo : direntry -> bool **)

    let is_fifo = Mlcpp_filesystem.Filesystem.Direntry.is_fifo

    (** val is_other : direntry -> bool **)

    let is_other = Mlcpp_filesystem.Filesystem.Direntry.is_other

    (** val is_socket : direntry -> bool **)

    let is_socket = Mlcpp_filesystem.Filesystem.Direntry.is_socket

    (** val is_symlink : direntry -> bool **)

    let is_symlink = Mlcpp_filesystem.Filesystem.Direntry.is_symlink

    (** val file_size : direntry -> n **)

    let file_size = fun de -> Mlcpp_filesystem.Filesystem.Direntry.file_size de |> Conversion.i2n

    (** val hard_link_count : direntry -> n **)

    let hard_link_count = fun de -> Mlcpp_filesystem.Filesystem.Direntry.hard_link_count de |> Conversion.i2n
   end

  (** val list_directory : path -> 'a1 -> (direntry -> 'a1 -> 'a1) -> 'a1 **)

  let list_directory = Mlcpp_filesystem.Filesystem.list_directory
 end

module Utilities =
 struct
  (** val make_list : positive -> positive list **)

  let make_list n0 =
    let natlist = seq (S O) (Coq_Pos.to_nat n0) in map Coq_Pos.of_nat natlist

  (** val rnd : n -> n **)

  let rnd = 
    function
     _ -> Elykseer_crypto.Random.with_rng (fun rng -> Elykseer_crypto.Random.random32 rng) |> Conversion.i2n
   

  (** val rnd256 : string -> string **)

  let rnd256 = 
   function
   x -> Elykseer_crypto.Random.with_rng (fun rng -> Elykseer_crypto.Random.random32 rng) |> string_of_int |>
     String.cat x |>
     String.cat (Unix.gethostname ()) |> String.cat (Unix.gettimeofday () |> string_of_float) |>
     Elykseer_crypto.Sha256.string
   

  (** val sha256 : string -> string **)

  let sha256 = Elykseer_crypto.Sha256.string
 end

module Assembly =
 struct
  (** val chunkwidth : positive **)

  let chunkwidth =
    XO (XO (XO (XO (XO (XO (XO (XO XH)))))))

  (** val chunklength : positive **)

  let chunklength =
    XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))

  (** val chunksize : positive **)

  let chunksize =
    Coq_Pos.mul chunkwidth chunklength

  (** val chunksize_N : n **)

  let chunksize_N =
    Conversion.pos2N chunksize

  (** val assemblysize : Nchunks.Private.t -> n **)

  let assemblysize n0 =
    N.mul chunksize_N (Nchunks.to_N n0)

  type aid_t = string

  type assemblyinformation = { nchunks : Nchunks.Private.t; aid : aid_t;
                               apos : n }

  (** val nchunks : assemblyinformation -> Nchunks.Private.t **)

  let nchunks a =
    a.nchunks

  (** val aid : assemblyinformation -> aid_t **)

  let aid a =
    a.aid

  (** val apos : assemblyinformation -> n **)

  let apos a =
    a.apos

  (** val etaX : assemblyinformation settable **)

  let etaX x =
    { nchunks = x.nchunks; aid = x.aid; apos = x.apos }

  type keyinformation = { ivec : string; pkey : string; localid : string;
                          localnchunks : positive }

  (** val ivec : keyinformation -> string **)

  let ivec k =
    k.ivec

  (** val pkey : keyinformation -> string **)

  let pkey k =
    k.pkey

  (** val localid : keyinformation -> string **)

  let localid k =
    k.localid

  (** val localnchunks : keyinformation -> positive **)

  let localnchunks k =
    k.localnchunks

  type blockinformation = { blockid : positive; bchecksum : string;
                            blocksize : n; filepos : n; blockaid : aid_t;
                            blockapos : n }

  (** val blockid : blockinformation -> positive **)

  let blockid b =
    b.blockid

  (** val bchecksum : blockinformation -> string **)

  let bchecksum b =
    b.bchecksum

  (** val blocksize : blockinformation -> n **)

  let blocksize b =
    b.blocksize

  (** val filepos : blockinformation -> n **)

  let filepos b =
    b.filepos

  (** val blockaid : blockinformation -> aid_t **)

  let blockaid b =
    b.blockaid

  (** val blockapos : blockinformation -> n **)

  let blockapos b =
    b.blockapos

  module type ASS =
   sig
    type coq_B

    val create : Configuration.configuration -> assemblyinformation * coq_B

    val buffer_len : coq_B -> n

    val calc_checksum : coq_B -> string
   end

  module AssemblyPlainWritable =
   struct
    type coq_B = Cstdio.BufferPlain.buffer_t

    (** val buffer_len : coq_B -> n **)

    let buffer_len =
      Cstdio.BufferPlain.buffer_len

    (** val calc_checksum : coq_B -> string **)

    let calc_checksum _ =
      "<>"

    (** val create :
        Configuration.configuration -> assemblyinformation * coq_B **)

    let create c =
      let chunks = c.Configuration.config_nchunks in
      let b =
        Cstdio.BufferPlain.buffer_create
          (N.mul chunksize_N (Nchunks.to_N chunks))
      in
      let rb = Cstdio.ranbuf128 () in
      let nb =
        Cstdio.BufferPlain.copy_sz_pos rb N0 (Npos (XO (XO (XO (XO XH))))) b
          N0
      in
      ({ nchunks = chunks; aid = (Utilities.rnd256 c.Configuration.my_id);
      apos = nb }, b)
   end

  module AssemblyEncrypted =
   struct
    type coq_B = Cstdio.BufferEncrypted.buffer_t

    (** val buffer_len : coq_B -> n **)

    let buffer_len =
      Cstdio.BufferEncrypted.buffer_len

    (** val calc_checksum : coq_B -> string **)

    let calc_checksum =
      Cstdio.BufferEncrypted.calc_checksum

    (** val create :
        Configuration.configuration -> assemblyinformation * coq_B **)

    let create c =
      let chunks = c.Configuration.config_nchunks in
      let b =
        Cstdio.BufferEncrypted.buffer_create
          (N.mul chunksize_N (Nchunks.to_N chunks))
      in
      ({ nchunks = chunks; aid = (Utilities.rnd256 c.Configuration.my_id);
      apos = N0 }, b)
   end

  module AssemblyPlainFull =
   struct
    type coq_B = Cstdio.BufferPlain.buffer_t

    (** val buffer_len : coq_B -> n **)

    let buffer_len =
      Cstdio.BufferPlain.buffer_len

    (** val calc_checksum : coq_B -> string **)

    let calc_checksum =
      Cstdio.BufferPlain.calc_checksum

    (** val create :
        Configuration.configuration -> assemblyinformation * coq_B **)

    let create c =
      let chunks = c.Configuration.config_nchunks in
      let sz = N.mul chunksize_N (Nchunks.to_N chunks) in
      let b = Cstdio.BufferPlain.buffer_create sz in
      ({ nchunks = chunks; aid = (Utilities.rnd256 c.Configuration.my_id);
      apos = sz }, b)
   end

  (** val id_assembly_full_ainfo_from_writable :
      assemblyinformation -> assemblyinformation **)

  let id_assembly_full_ainfo_from_writable = fun b -> Helper.cpp_buffer_id b

  (** val id_assembly_full_buffer_from_writable :
      AssemblyPlainWritable.coq_B -> AssemblyPlainFull.coq_B **)

  let id_assembly_full_buffer_from_writable = fun b -> Helper.cpp_buffer_id b

  (** val finish :
      assemblyinformation -> AssemblyPlainWritable.coq_B ->
      assemblyinformation * AssemblyPlainFull.coq_B **)

  let finish a b =
    ((id_assembly_full_ainfo_from_writable a),
      (id_assembly_full_buffer_from_writable b))

  (** val assembly_add_content :
      Cstdio.BufferPlain.buffer_t -> n -> n -> AssemblyPlainWritable.coq_B ->
      n **)

  let assembly_add_content = 
    fun src sz_N pos_N tgt ->
      let sz = Conversion.n2i sz_N
      and pos = Conversion.n2i pos_N in
      Elykseer_base.Assembly.add_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt |> Conversion.i2n
   

  (** val backup :
      assemblyinformation -> AssemblyPlainWritable.coq_B -> n ->
      Cstdio.BufferPlain.buffer_t -> assemblyinformation * blockinformation **)

  let backup a b fpos content =
    let apos_n = a.apos in
    let bsz = Cstdio.BufferPlain.buffer_len content in
    let chksum = Cstdio.BufferPlain.calc_checksum content in
    let nwritten = assembly_add_content content bsz apos_n b in
    let bi = { blockid = XH; bchecksum = chksum; blocksize = nwritten;
      filepos = fpos; blockaid = a.aid; blockapos = apos_n }
    in
    let a' =
      set (fun a0 -> a0.apos) (fun f ->
        let n0 = fun r -> f r.apos in
        (fun x -> { nchunks = x.nchunks; aid = x.aid; apos = (n0 x) }))
        (fun ap -> N.add ap nwritten) a
    in
    (a', bi)

  (** val id_buffer_t_from_full :
      AssemblyPlainFull.coq_B -> Cstdio.BufferPlain.buffer_t **)

  let id_buffer_t_from_full = fun b -> Helper.cpp_buffer_id b

  (** val id_assembly_enc_buffer_t_from_buf :
      Cstdio.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B **)

  let id_assembly_enc_buffer_t_from_buf = fun b -> Helper.cpp_buffer_id b

  (** val encrypt :
      assemblyinformation -> AssemblyPlainFull.coq_B -> keyinformation ->
      (assemblyinformation * AssemblyEncrypted.coq_B) option **)

  let encrypt a b ki =
    let a' =
      set (fun a0 -> a0.apos) (fun f ->
        let n0 = fun r -> f r.apos in
        (fun x -> { nchunks = x.nchunks; aid = x.aid; apos = (n0 x) }))
        (const (assemblysize a.nchunks)) a
    in
    let benc = Cstdio.encrypt (id_buffer_t_from_full b) ki.ivec ki.pkey in
    let b' = id_assembly_enc_buffer_t_from_buf benc in Some (a', b')

  (** val assembly_get_content :
      AssemblyPlainFull.coq_B -> n -> n -> Cstdio.BufferPlain.buffer_t -> n **)

  let assembly_get_content = 
    fun src sz_N pos_N tgt ->
      let sz = Conversion.n2i sz_N
      and pos = Conversion.n2i pos_N in
      Elykseer_base.Assembly.get_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt |> Conversion.i2n
   

  (** val restore :
      AssemblyPlainFull.coq_B -> blockinformation ->
      Cstdio.BufferPlain.buffer_t option **)

  let restore b bi =
    let bsz = bi.blocksize in
    let b' = Cstdio.BufferPlain.buffer_create bsz in
    let nw = assembly_get_content b bsz bi.blockapos b' in
    if N.eqb nw bsz
    then let bcksum = Cstdio.BufferPlain.calc_checksum b' in
         if (=) bcksum bi.bchecksum then Some b' else None
    else None

  (** val id_buffer_t_from_enc :
      AssemblyEncrypted.coq_B -> Cstdio.BufferEncrypted.buffer_t **)

  let id_buffer_t_from_enc = fun b -> Helper.cpp_buffer_id b

  (** val id_assembly_plain_buffer_t_from_buf :
      Cstdio.BufferPlain.buffer_t -> AssemblyPlainFull.coq_B **)

  let id_assembly_plain_buffer_t_from_buf = fun b -> Helper.cpp_buffer_id b

  (** val decrypt :
      assemblyinformation -> AssemblyEncrypted.coq_B -> keyinformation ->
      (assemblyinformation * AssemblyPlainFull.coq_B) option **)

  let decrypt a b ki =
    let a' =
      set (fun a0 -> a0.apos) (fun f ->
        let n0 = fun r -> f r.apos in
        (fun x -> { nchunks = x.nchunks; aid = x.aid; apos = (n0 x) }))
        (const N0) a
    in
    let bdec = Cstdio.decrypt (id_buffer_t_from_enc b) ki.ivec ki.pkey in
    let b' = id_assembly_plain_buffer_t_from_buf bdec in Some (a', b')

  (** val chunk_identifier :
      Configuration.configuration -> aid_t -> positive -> string **)

  let chunk_identifier = fun config aid cid ->
      let s = (Configuration.my_id config) ^
              (string_of_int (Conversion.p2i cid)) ^
              aid in
      Elykseer_crypto.Sha256.string s
   

  (** val chunk_identifier_path :
      Configuration.configuration -> aid_t -> positive -> string **)

  let chunk_identifier_path = fun config aid cid -> let cident = chunk_identifier config aid cid in
      let subd = Helper.mk_cid_subdir cident in 
      (Configuration.path_chunks config ^ "/" ^ subd ^ "/" ^ cident ^ ".lxr")
   

  (** val load_chunk_from_path :
      string -> Cstdio.BufferEncrypted.buffer_t option **)

  let load_chunk_from_path sfp =
    if Filesystem.Path.file_exists (Filesystem.Path.from_string sfp)
    then let filtered_var = Cstdio.fopen sfp Cstdio.read_mode in
         (match filtered_var with
          | Some fptr0 ->
            let filtered_var0 = Cstdio.fread fptr0 chunksize_N in
            (match filtered_var0 with
             | Some p ->
               let (cnt, b) = p in
               let filtered_var1 = Cstdio.fclose fptr0 in
               (match filtered_var1 with
                | Some _ ->
                  if N.eqb cnt chunksize_N
                  then Some (Cstdio.BufferEncrypted.from_buffer b)
                  else None
                | None -> None)
             | None -> None)
          | None -> None)
    else None

  (** val id_enc_from_buffer_t :
      Cstdio.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B **)

  let id_enc_from_buffer_t = fun b -> Helper.cpp_buffer_id b

  (** val recall :
      Configuration.configuration -> assemblyinformation ->
      (assemblyinformation * AssemblyEncrypted.coq_B) option **)

  let recall c a =
    let cidlist = Utilities.make_list a.nchunks in
    let b =
      Cstdio.BufferEncrypted.buffer_create
        (N.mul (Conversion.pos2N a.nchunks) chunksize_N)
    in
    let aid0 = a.aid in
    let blen = Cstdio.BufferEncrypted.buffer_len b in
    let nread =
      fold_left (fun nread cid ->
        let cpath = chunk_identifier_path c aid0 cid in
        let filtered_var = load_chunk_from_path cpath in
        (match filtered_var with
         | Some cb ->
           let apos0 =
             N.mul chunksize_N (N.sub (Conversion.pos2N cid) (Npos XH))
           in
           if N.leb (N.add apos0 chunksize_N) blen
           then N.add nread
                  (Cstdio.BufferEncrypted.copy_sz_pos cb N0 chunksize_N b
                    apos0)
           else nread
         | None -> nread)) cidlist N0
    in
    let a' =
      set (fun a0 -> a0.apos) (fun f ->
        let n0 = fun r -> f r.apos in
        (fun x -> { nchunks = x.nchunks; aid = x.aid; apos = (n0 x) }))
        (const nread) a
    in
    let b' = id_enc_from_buffer_t b in
    if N.eqb nread blen then Some (a', b') else None

  (** val store_chunk_to_path :
      string -> n -> n -> Cstdio.BufferEncrypted.buffer_t -> n **)

  let store_chunk_to_path sfp sz pos b =
    let fp = Filesystem.Path.from_string sfp in
    if Filesystem.Path.file_exists fp
    then N0
    else let dir = Filesystem.Path.parent fp in
         if (||) (Filesystem.Path.is_directory dir)
              (Filesystem.create_directories dir)
         then let filtered_var = Cstdio.fopen sfp Cstdio.write_new_mode in
              (match filtered_var with
               | Some fptr0 ->
                 let buf = Cstdio.BufferEncrypted.buffer_create sz in
                 if N.ltb N0
                      (Cstdio.BufferEncrypted.copy_sz_pos b pos sz buf N0)
                 then let res =
                        let filtered_var0 =
                          Cstdio.fwrite fptr0 sz
                            (Cstdio.BufferEncrypted.to_buffer buf)
                        in
                        (match filtered_var0 with
                         | Some cnt -> cnt
                         | None -> N0)
                      in
                      let filtered_var0 = Cstdio.fflush fptr0 in
                      (match filtered_var0 with
                       | Some fptr2 ->
                         let filtered_var1 = Cstdio.fclose fptr2 in
                         (match filtered_var1 with
                          | Some _ -> res
                          | None -> N0)
                       | None -> res)
                 else let filtered_var0 = Cstdio.fclose fptr0 in
                      (match filtered_var0 with
                       | Some _ ->
                         N.sub (N.sub (Npos (XO XH)) (Npos XH)) (Npos XH)
                       | None -> N0)
               | None -> N0)
         else N0

  (** val extract :
      Configuration.configuration -> assemblyinformation ->
      AssemblyEncrypted.coq_B -> n **)

  let extract c a b =
    let aid0 = a.aid in
    let buf = id_buffer_t_from_enc b in
    fold_left (fun nwritten cid ->
      let cpath = chunk_identifier_path c aid0 cid in
      let apos0 = N.mul chunksize_N (N.sub (Conversion.pos2N cid) (Npos XH))
      in
      N.add nwritten (store_chunk_to_path cpath chunksize_N apos0 buf))
      (Utilities.make_list a.nchunks) N0
 end

module Filesupport =
 struct
  type filename = string

  type fileinformation = { fname : filename; fsize : n; fowner : string;
                           fpermissions : n; fmodified : string;
                           fchecksum : string }

  (** val fname : fileinformation -> filename **)

  let fname f =
    f.fname

  (** val fsize : fileinformation -> n **)

  let fsize f =
    f.fsize

  (** val fowner : fileinformation -> string **)

  let fowner f =
    f.fowner

  (** val fpermissions : fileinformation -> n **)

  let fpermissions f =
    f.fpermissions

  (** val fmodified : fileinformation -> string **)

  let fmodified f =
    f.fmodified

  (** val fchecksum : fileinformation -> string **)

  let fchecksum f =
    f.fchecksum

  (** val get_file_information : filename -> fileinformation **)

  let get_file_information =   
    fun fn ->
        { fname = fn;
          fsize = Conversion.i2n (Elykseer_base.Fsutils.fsize fn);
          fowner = string_of_int (Elykseer_base.Fsutils.fowner fn);
          fpermissions = Conversion.i2n (Elykseer_base.Fsutils.fperm fn);
          fmodified = Elykseer_base.Fsutils.fmod fn;
          fchecksum = Elykseer_base.Fsutils.fchksum fn }
   
 end

module Store =
 struct
  type 'kVs store = { sconfig : Configuration.configuration; entries : 'kVs }

  (** val sconfig : 'a1 store -> Configuration.configuration **)

  let sconfig s =
    s.sconfig

  (** val entries : 'a1 store -> 'a1 **)

  let entries s =
    s.entries

  (** val rec_find : string -> (string * 'a1) list -> 'a1 option **)

  let rec rec_find k = function
  | [] -> None
  | p :: r -> let (k', v') = p in if (=) k' k then Some v' else rec_find k r

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

  module KeyListStore =
   struct
    type coq_K = string

    type coq_V = Assembly.keyinformation

    type coq_KVs = (coq_K * coq_V) list

    type coq_R = coq_KVs store

    (** val init : Configuration.configuration -> coq_R **)

    let init c =
      { sconfig = c; entries = [] }

    (** val add : coq_K -> coq_V -> coq_R -> coq_R **)

    let add k v r =
      { sconfig = r.sconfig; entries = ((k, v) :: r.entries) }

    (** val find : coq_K -> coq_R -> coq_V option **)

    let find k r =
      rec_find k r.entries
   end

  module FBlockListStore =
   struct
    type coq_K = Assembly.aid_t

    type coq_V = Assembly.blockinformation

    type coq_KVs = (coq_K * coq_V) list

    type coq_R = coq_KVs store

    (** val init : Configuration.configuration -> coq_R **)

    let init c =
      { sconfig = c; entries = [] }

    (** val add : coq_K -> coq_V -> coq_R -> coq_R **)

    let add k v r =
      { sconfig = r.sconfig; entries = ((k, v) :: r.entries) }

    (** val find : coq_K -> coq_R -> coq_V option **)

    let find k r =
      rec_find k r.entries
   end

  module FileinformationStore =
   struct
    type coq_K = string

    type coq_V = Filesupport.fileinformation

    type coq_KVs = (coq_K * coq_V) list

    type coq_R = coq_KVs store

    (** val init : Configuration.configuration -> coq_R **)

    let init c =
      { sconfig = c; entries = [] }

    (** val add : coq_K -> coq_V -> coq_R -> coq_R **)

    let add k v r =
      { sconfig = r.sconfig; entries = ((k, v) :: r.entries) }

    (** val find : coq_K -> coq_R -> coq_V option **)

    let find k r =
      rec_find k r.entries
   end
 end

module Environment =
 struct
  type 'aB environment = { cur_assembly : Assembly.assemblyinformation;
                           cur_buffer : 'aB;
                           econfig : Configuration.configuration }

  (** val cur_assembly : 'a1 environment -> Assembly.assemblyinformation **)

  let cur_assembly e =
    e.cur_assembly

  (** val cur_buffer : 'a1 environment -> 'a1 **)

  let cur_buffer e =
    e.cur_buffer

  (** val econfig : 'a1 environment -> Configuration.configuration **)

  let econfig e =
    e.econfig

  (** val cpp_mk_key256 : unit -> string **)

  let cpp_mk_key256 = fun () -> Elykseer_crypto.Key256.mk () |> Elykseer_crypto.Key256.to_hex

  (** val cpp_mk_key128 : unit -> string **)

  let cpp_mk_key128 = fun () -> Elykseer_crypto.Key128.mk () |> Elykseer_crypto.Key128.to_hex

  module type ENV =
   sig
    type coq_AB

    type coq_E = coq_AB environment

    val initial_environment : Configuration.configuration -> coq_E
   end

  module EnvironmentWritable =
   struct
    type coq_AB = Assembly.AssemblyPlainWritable.coq_B

    type coq_E = coq_AB environment

    (** val initial_environment : Configuration.configuration -> coq_E **)

    let initial_environment c =
      let (a, b) = Assembly.AssemblyPlainWritable.create c in
      { cur_assembly = a; cur_buffer = b; econfig = c }

    (** val recreate_assembly : coq_AB environment -> coq_AB environment **)

    let recreate_assembly e =
      let (a, b) = Assembly.AssemblyPlainWritable.create e.econfig in
      { cur_assembly = a; cur_buffer = b; econfig = e.econfig }

    (** val finalise_assembly :
        coq_AB environment -> (Assembly.aid_t * Assembly.keyinformation)
        option **)

    let finalise_assembly e0 =
      let a0 = e0.cur_assembly in
      let apos0 = a0.Assembly.apos in
      if N.ltb N0 apos0
      then let (a, b) = Assembly.finish a0 e0.cur_buffer in
           let ki = { Assembly.ivec = (cpp_mk_key128 ()); Assembly.pkey =
             (cpp_mk_key256 ()); Assembly.localid =
             e0.econfig.Configuration.my_id; Assembly.localnchunks =
             e0.econfig.Configuration.config_nchunks }
           in
           (match Assembly.encrypt a b ki with
            | Some p ->
              let (a', b') = p in
              let n0 = Assembly.extract e0.econfig a' b' in
              if N.ltb N0 n0 then Some (a.Assembly.aid, ki) else None
            | None -> None)
      else None

    (** val finalise_and_recreate_assembly :
        coq_AB environment -> (coq_AB
        environment * (Assembly.aid_t * Assembly.keyinformation)) option **)

    let finalise_and_recreate_assembly e0 =
      match finalise_assembly e0 with
      | Some ki -> Some ((recreate_assembly e0), ki)
      | None -> None

    (** val backup :
        coq_AB environment -> string -> n -> Cstdio.BufferPlain.buffer_t ->
        coq_AB
        environment * (Assembly.blockinformation * (Assembly.aid_t * Assembly.keyinformation)
        option) **)

    let backup e0 _ fpos content =
      let afree =
        N.sub (Assembly.assemblysize e0.econfig.Configuration.config_nchunks)
          e0.cur_assembly.Assembly.apos
      in
      let blen = Cstdio.BufferPlain.buffer_len content in
      let (ki, e1) =
        if N.ltb afree blen
        then let filtered_var = finalise_and_recreate_assembly e0 in
             (match filtered_var with
              | Some p -> let (e0', ki) = p in ((Some ki), e0')
              | None -> (None, e0))
        else (None, e0)
      in
      let (a', bi) =
        Assembly.backup e1.cur_assembly e1.cur_buffer fpos content
      in
      ({ cur_assembly = a'; cur_buffer = e1.cur_buffer; econfig =
      e1.econfig }, (bi, ki))
   end

  module EnvironmentReadable =
   struct
    type coq_AB = Assembly.AssemblyPlainFull.coq_B

    type coq_E = coq_AB environment

    (** val initial_environment : Configuration.configuration -> coq_E **)

    let initial_environment c =
      let (a, b) = Assembly.AssemblyPlainFull.create c in
      { cur_assembly = a; cur_buffer = b; econfig = c }

    (** val restore_assembly :
        coq_AB environment -> Assembly.aid_t -> Assembly.keyinformation ->
        coq_AB environment option **)

    let restore_assembly e0 aid0 ki =
      match Assembly.recall e0.econfig { Assembly.nchunks =
              e0.econfig.Configuration.config_nchunks; Assembly.aid = aid0;
              Assembly.apos = N0 } with
      | Some p ->
        let (a1, b1) = p in
        (match Assembly.decrypt a1 b1 ki with
         | Some p0 ->
           let (a2, b2) = p0 in
           Some { cur_assembly = a2; cur_buffer = b2; econfig = e0.econfig }
         | None -> None)
      | None -> None
   end
 end

module AssemblyCache =
 struct
  type readqueueentity = { rqaid : Assembly.aid_t; rqapos : n; rqrlen : 
                           n; rqfpos : n }

  (** val rqaid : readqueueentity -> Assembly.aid_t **)

  let rqaid r =
    r.rqaid

  (** val rqapos : readqueueentity -> n **)

  let rqapos r =
    r.rqapos

  (** val rqrlen : readqueueentity -> n **)

  let rqrlen r =
    r.rqrlen

  (** val rqfpos : readqueueentity -> n **)

  let rqfpos r =
    r.rqfpos

  type readqueueresult = { readrequest : readqueueentity;
                           rresult : Cstdio.BufferPlain.buffer_t }

  (** val readrequest : readqueueresult -> readqueueentity **)

  let readrequest r =
    r.readrequest

  (** val rresult : readqueueresult -> Cstdio.BufferPlain.buffer_t **)

  let rresult r =
    r.rresult

  type writequeueentity = { qfhash : string; qfpos : n;
                            qbuffer : Cstdio.BufferPlain.buffer_t }

  (** val qfhash : writequeueentity -> string **)

  let qfhash w =
    w.qfhash

  (** val qfpos : writequeueentity -> n **)

  let qfpos w =
    w.qfpos

  (** val qbuffer : writequeueentity -> Cstdio.BufferPlain.buffer_t **)

  let qbuffer w =
    w.qbuffer

  type writequeueresult = { writerequest : writequeueentity;
                            wresult : readqueueentity }

  (** val writerequest : writequeueresult -> writequeueentity **)

  let writerequest w =
    w.writerequest

  (** val wresult : writequeueresult -> readqueueentity **)

  let wresult w =
    w.wresult

  (** val qsize : positive **)

  let qsize =
    XO (XO (XI XH))

  type readqueue = { rqueue : readqueueentity list; rqueuesz : positive }

  (** val rqueue : readqueue -> readqueueentity list **)

  let rqueue r =
    r.rqueue

  (** val rqueuesz : readqueue -> positive **)

  let rqueuesz r =
    r.rqueuesz

  type writequeue = { wqueue : writequeueentity list; wqueuesz : positive }

  (** val wqueue : writequeue -> writequeueentity list **)

  let wqueue w =
    w.wqueue

  (** val wqueuesz : writequeue -> positive **)

  let wqueuesz w =
    w.wqueuesz

  type assemblycache = { acenvs : Environment.EnvironmentReadable.coq_E list;
                         acsize : nat;
                         acwriteenv : Environment.EnvironmentWritable.coq_E;
                         acconfig : Configuration.configuration;
                         acwriteq : writequeue; acreadq : readqueue;
                         acfbstore : Store.FBlockListStore.coq_R;
                         ackstore : Store.KeyListStore.coq_R }

  (** val acenvs :
      assemblycache -> Environment.EnvironmentReadable.coq_E list **)

  let acenvs a =
    a.acenvs

  (** val acsize : assemblycache -> nat **)

  let acsize a =
    a.acsize

  (** val acwriteenv :
      assemblycache -> Environment.EnvironmentWritable.coq_E **)

  let acwriteenv a =
    a.acwriteenv

  (** val acconfig : assemblycache -> Configuration.configuration **)

  let acconfig a =
    a.acconfig

  (** val acwriteq : assemblycache -> writequeue **)

  let acwriteq a =
    a.acwriteq

  (** val acreadq : assemblycache -> readqueue **)

  let acreadq a =
    a.acreadq

  (** val acfbstore : assemblycache -> Store.FBlockListStore.coq_R **)

  let acfbstore a =
    a.acfbstore

  (** val ackstore : assemblycache -> Store.KeyListStore.coq_R **)

  let ackstore a =
    a.ackstore

  (** val prepare_assemblycache :
      Configuration.configuration -> positive -> assemblycache **)

  let prepare_assemblycache c size =
    { acenvs = []; acsize = (Coq_Pos.to_nat size); acwriteenv =
      (Environment.EnvironmentWritable.initial_environment c); acconfig = c;
      acwriteq = { wqueue = []; wqueuesz = qsize }; acreadq = { rqueue = [];
      rqueuesz = qsize }; acfbstore = (Store.FBlockListStore.init c);
      ackstore = (Store.KeyListStore.init c) }

  (** val enqueue_write_request :
      assemblycache -> writequeueentity -> bool * assemblycache **)

  let enqueue_write_request ac req =
    let wq = ac.acwriteq.wqueue in
    let ln = length wq in
    if N.leb (Conversion.pos2N qsize) (Conversion.nat2N ln)
    then (false, ac)
    else (true, { acenvs = ac.acenvs; acsize = ac.acsize; acwriteenv =
           ac.acwriteenv; acconfig = ac.acconfig; acwriteq = { wqueue =
           (req :: wq); wqueuesz = ac.acwriteq.wqueuesz }; acreadq =
           ac.acreadq; acfbstore = ac.acfbstore; ackstore = ac.ackstore })

  (** val enqueue_read_request :
      assemblycache -> readqueueentity -> bool * assemblycache **)

  let enqueue_read_request ac req =
    let rq = ac.acreadq.rqueue in
    let ln = length rq in
    if N.leb (Conversion.pos2N qsize) (Conversion.nat2N ln)
    then (false, ac)
    else (true, { acenvs = ac.acenvs; acsize = ac.acsize; acwriteenv =
           ac.acwriteenv; acconfig = ac.acconfig; acwriteq = ac.acwriteq;
           acreadq = { rqueue = (req :: rq); rqueuesz =
           ac.acreadq.rqueuesz }; acfbstore = ac.acfbstore; ackstore =
           ac.ackstore })

  (** val try_restore_assembly :
      assemblycache -> Assembly.aid_t ->
      Environment.EnvironmentReadable.coq_E option **)

  let try_restore_assembly ac sel_aid =
    let filtered_var = Store.KeyListStore.find sel_aid ac.ackstore in
    (match filtered_var with
     | Some ki ->
       Environment.EnvironmentReadable.restore_assembly
         (Environment.EnvironmentReadable.initial_environment ac.acconfig)
         sel_aid ki
     | None -> None)

  (** val set_envs :
      assemblycache -> Environment.EnvironmentReadable.coq_E list ->
      assemblycache **)

  let set_envs ac0 envs =
    { acenvs = envs; acsize = ac0.acsize; acwriteenv = ac0.acwriteenv;
      acconfig = ac0.acconfig; acwriteq = ac0.acwriteq; acreadq =
      ac0.acreadq; acfbstore = ac0.acfbstore; ackstore = ac0.ackstore }

  (** val ensure_assembly :
      assemblycache -> Assembly.aid_t ->
      (Environment.EnvironmentReadable.coq_E * assemblycache) option **)

  let ensure_assembly ac0 sel_aid =
    let filtered_var = ac0.acenvs in
    (match filtered_var with
     | [] ->
       let filtered_var0 = try_restore_assembly ac0 sel_aid in
       (match filtered_var0 with
        | Some env -> Some (env, (set_envs ac0 (env :: [])))
        | None -> None)
     | e1 :: r ->
       (match r with
        | [] ->
          if (=) e1.Environment.cur_assembly.Assembly.aid sel_aid
          then Some (e1, ac0)
          else let filtered_var0 = try_restore_assembly ac0 sel_aid in
               (match filtered_var0 with
                | Some env ->
                  if eqb ac0.acsize (S O)
                  then Some (env, (set_envs ac0 (env :: [])))
                  else Some (env, (set_envs ac0 (env :: (e1 :: []))))
                | None -> None)
        | e :: l ->
          let r0 = e :: l in
          if (=) e1.Environment.cur_assembly.Assembly.aid sel_aid
          then Some (e1, ac0)
          else let found =
                 filter (fun e0 ->
                   (=) e0.Environment.cur_assembly.Assembly.aid sel_aid) r0
               in
               (match found with
                | [] ->
                  let filtered_var0 = try_restore_assembly ac0 sel_aid in
                  (match filtered_var0 with
                   | Some env ->
                     let lr =
                       if leb ac0.acsize (length ac0.acenvs)
                       then removelast ac0.acenvs
                       else ac0.acenvs
                     in
                     Some (env, (set_envs ac0 (env :: lr)))
                   | None -> None)
                | efound :: _ ->
                  let r' =
                    filter (fun e0 ->
                      negb
                        ((=) e0.Environment.cur_assembly.Assembly.aid sel_aid))
                      r0
                  in
                  Some (efound, (set_envs ac0 (efound :: (e1 :: r')))))))

  (** val run_read_requests :
      assemblycache -> readqueueentity list -> readqueueresult list ->
      readqueueresult list * assemblycache **)

  let rec run_read_requests ac0 reqs res =
    match reqs with
    | [] -> (res, ac0)
    | h :: r ->
      let aid0 = h.rqaid in
      let filtered_var = ensure_assembly ac0 aid0 in
      (match filtered_var with
       | Some p ->
         let (env, ac1) = p in
         let buf = Cstdio.BufferPlain.buffer_create h.rqrlen in
         let n0 =
           Assembly.assembly_get_content env.Environment.cur_buffer h.rqrlen
             h.rqapos buf
         in
         let res' =
           if N.ltb N0 n0
           then { readrequest = h; rresult = buf } :: res
           else res
         in
         run_read_requests ac1 r res'
       | None -> (res, ac0))

  (** val run_write_requests :
      assemblycache -> writequeueentity list -> writequeueresult list ->
      writequeueresult list * assemblycache **)

  let rec run_write_requests ac0 reqs res =
    match reqs with
    | [] -> (res, ac0)
    | h :: r ->
      let fp = h.qfhash in
      let filtered_var =
        Environment.EnvironmentWritable.backup ac0.acwriteenv fp h.qfpos
          h.qbuffer
      in
      let (env, p) = filtered_var in
      let (bi, kis) = p in
      let ackstore' =
        match kis with
        | Some p0 ->
          let (aid0, ki) = p0 in Store.KeyListStore.add aid0 ki ac0.ackstore
        | None -> ac0.ackstore
      in
      let ac1 = { acenvs = ac0.acenvs; acsize = ac0.acsize; acwriteenv = env;
        acconfig = ac0.acconfig; acwriteq = { wqueue = []; wqueuesz =
        ac0.acwriteq.wqueuesz }; acreadq = ac0.acreadq; acfbstore =
        (Store.FBlockListStore.add fp bi ac0.acfbstore); ackstore =
        ackstore' }
      in
      run_write_requests ac1 r ({ writerequest = h; wresult = { rqaid =
        env.Environment.cur_assembly.Assembly.aid; rqapos =
        bi.Assembly.blockapos; rqrlen =
        (Cstdio.BufferPlain.buffer_len h.qbuffer); rqfpos =
        h.qfpos } } :: res)

  (** val iterate_read_queue :
      assemblycache -> readqueueresult list * assemblycache **)

  let iterate_read_queue ac0 =
    let filtered_var = ac0.acreadq.rqueue in
    (match filtered_var with
     | [] -> ([], ac0)
     | h :: r ->
       let aid0 = h.rqaid in
       let sel = filter (fun e -> (=) e.rqaid aid0) r in
       let ac1 = { acenvs = ac0.acenvs; acsize = ac0.acsize; acwriteenv =
         ac0.acwriteenv; acconfig = ac0.acconfig; acwriteq = ac0.acwriteq;
         acreadq = { rqueue = (filter (fun e -> negb ((=) e.rqaid aid0)) r);
         rqueuesz = ac0.acreadq.rqueuesz }; acfbstore = ac0.acfbstore;
         ackstore = ac0.ackstore }
       in
       run_read_requests ac1 (h :: sel) [])

  (** val iterate_write_queue :
      assemblycache -> writequeueresult list * assemblycache **)

  let iterate_write_queue ac0 =
    let filtered_var = ac0.acwriteq.wqueue in
    (match filtered_var with
     | [] -> ([], ac0)
     | h :: r ->
       let ac1 = { acenvs = ac0.acenvs; acsize = ac0.acsize; acwriteenv =
         ac0.acwriteenv; acconfig = ac0.acconfig; acwriteq = { wqueue = [];
         wqueuesz = ac0.acwriteq.wqueuesz }; acreadq = ac0.acreadq;
         acfbstore = ac0.acfbstore; ackstore = ac0.ackstore }
       in
       run_write_requests ac1 (h :: r) [])

  (** val flush : assemblycache -> assemblycache **)

  let flush ac0 =
    let filtered_var =
      Environment.EnvironmentWritable.finalise_and_recreate_assembly
        ac0.acwriteenv
    in
    (match filtered_var with
     | Some p ->
       let (env', p0) = p in
       let (aid0, ki) = p0 in
       let ackstore' = Store.KeyListStore.add aid0 ki ac0.ackstore in
       { acenvs = []; acsize = ac0.acsize; acwriteenv = env'; acconfig =
       ac0.acconfig; acwriteq = ac0.acwriteq; acreadq = ac0.acreadq;
       acfbstore = ac0.acfbstore; ackstore = ackstore' }
     | None -> ac0)

  (** val close : assemblycache -> assemblycache **)

  let close ac0 =
    let filtered_var =
      Environment.EnvironmentWritable.finalise_assembly ac0.acwriteenv
    in
    (match filtered_var with
     | Some p ->
       let (aid0, ki) = p in
       let ackstore' = Store.KeyListStore.add aid0 ki ac0.ackstore in
       { acenvs = []; acsize = ac0.acsize; acwriteenv =
       (Environment.EnvironmentWritable.initial_environment ac0.acconfig);
       acconfig = ac0.acconfig; acwriteq = ac0.acwriteq; acreadq =
       ac0.acreadq; acfbstore = ac0.acfbstore; ackstore = ackstore' }
     | None -> ac0)

  (** val add_key :
      assemblycache -> Assembly.aid_t -> Assembly.keyinformation ->
      assemblycache **)

  let add_key ac aid0 ki =
    { acenvs = ac.acenvs; acsize = ac.acsize; acwriteenv = ac.acwriteenv;
      acconfig = ac.acconfig; acwriteq = ac.acwriteq; acreadq = ac.acreadq;
      acfbstore = ac.acfbstore; ackstore =
      (Store.KeyListStore.add aid0 ki ac.ackstore) }
 end

module BackupPlanner =
 struct
  type fileblock = { fbanum : positive; fbfpos : n; fbsz : n }

  (** val fbanum : fileblock -> positive **)

  let fbanum f =
    f.fbanum

  (** val fbfpos : fileblock -> n **)

  let fbfpos f =
    f.fbfpos

  (** val fbsz : fileblock -> n **)

  let fbsz f =
    f.fbsz

  type fileblockinformation = { fbifi : Filesupport.fileinformation;
                                fbifblocks : fileblock list }

  (** val fbifi : fileblockinformation -> Filesupport.fileinformation **)

  let fbifi f =
    f.fbifi

  (** val fbifblocks : fileblockinformation -> fileblock list **)

  let fbifblocks f =
    f.fbifblocks

  (** val aminsz : n **)

  let aminsz =
    Npos (XO (XO (XO (XO (XO (XO XH))))))

  (** val prepare_blocks :
      positive -> n -> nat -> positive -> n -> fileblock list -> n -> n ->
      fileblock list * (positive * n) **)

  let rec prepare_blocks nchunks0 maxsz fuel anum_p afree_p fbs_p fpos fsz =
    match fuel with
    | O -> (fbs_p, (anum_p, afree_p))
    | S f' ->
      if N.leb fsz afree_p
      then if N.leb maxsz fsz
           then let newblock = { fbanum = anum_p; fbfpos = fpos; fbsz =
                  maxsz }
                in
                prepare_blocks nchunks0 maxsz f' anum_p (N.sub afree_p maxsz)
                  (newblock :: fbs_p) (N.add fpos maxsz) (N.sub fsz maxsz)
           else if N.ltb N0 fsz
                then (({ fbanum = anum_p; fbfpos = fpos; fbsz =
                       fsz } :: fbs_p), (anum_p, (N.sub afree_p fsz)))
                else (fbs_p, (anum_p, afree_p))
      else if N.leb maxsz afree_p
           then let newblock = { fbanum = anum_p; fbfpos = fpos; fbsz =
                  maxsz }
                in
                prepare_blocks nchunks0 maxsz f' anum_p (N.sub afree_p maxsz)
                  (newblock :: fbs_p) (N.add fpos maxsz) (N.sub fsz maxsz)
           else let asz = Assembly.assemblysize nchunks0 in
                if N.ltb afree_p aminsz
                then prepare_blocks nchunks0 maxsz f' (Coq_Pos.add anum_p XH)
                       asz fbs_p fpos fsz
                else let newblock = { fbanum = anum_p; fbfpos = fpos; fbsz =
                       afree_p }
                     in
                     prepare_blocks nchunks0 maxsz f' (Coq_Pos.add anum_p XH)
                       asz (newblock :: fbs_p) (N.add fpos afree_p)
                       (N.sub fsz afree_p)

  (** val max_block_size : n **)

  let max_block_size =
    N.mul (Npos (XO (XO (XO (XO (XO (XO (XO XH)))))))) (Npos (XO (XO (XO (XO
      (XO (XO (XO (XO (XO (XO XH)))))))))))

  (** val analyse_file :
      positive -> n -> positive -> string ->
      (positive * n) * fileblockinformation **)

  let analyse_file nchunks0 afree_p anum_p fn =
    let fi = Filesupport.get_file_information fn in
    let nblocks = N.div fi.Filesupport.fsize max_block_size in
    let fuel =
      N.to_nat
        (N.add (N.add nblocks (N.div nblocks (Conversion.pos2N nchunks0)))
          (Npos XH))
    in
    let (afbs, ares) =
      prepare_blocks nchunks0 max_block_size fuel anum_p afree_p [] N0
        fi.Filesupport.fsize
    in
    (ares, { fbifi = fi; fbifblocks = (rev afbs) })
 end

module Processor =
 struct
  type processor = { config : Configuration.configuration;
                     cache : AssemblyCache.assemblycache }

  (** val config : processor -> Configuration.configuration **)

  let config p =
    p.config

  (** val cache : processor -> AssemblyCache.assemblycache **)

  let cache p =
    p.cache

  (** val cache_sz : positive **)

  let cache_sz =
    XI XH

  (** val prepare_processor : Configuration.configuration -> processor **)

  let prepare_processor c =
    { config = c; cache = (AssemblyCache.prepare_assemblycache c cache_sz) }

  (** val update_cache :
      processor -> AssemblyCache.assemblycache -> processor **)

  let update_cache this ac =
    { config = this.config; cache = ac }

  (** val backup_block :
      processor -> AssemblyCache.writequeueentity -> processor **)

  let backup_block this wqe =
    let filtered_var = AssemblyCache.enqueue_write_request this.cache wqe in
    let (b, cache') = filtered_var in
    if b
    then update_cache this cache'
    else let (_, cache'0) = AssemblyCache.iterate_write_queue this.cache in
         let filtered_var0 = AssemblyCache.enqueue_write_request cache'0 wqe
         in
         let (b0, cache'') = filtered_var0 in
         if b0 then update_cache this cache'' else this

  (** val request_read :
      processor -> AssemblyCache.readqueueentity ->
      AssemblyCache.readqueueresult list * processor **)

  let request_read this rqe =
    let filtered_var = AssemblyCache.enqueue_read_request this.cache rqe in
    let (b, cache') = filtered_var in
    if b
    then ([], (update_cache this cache'))
    else let (rres, cache'0) = AssemblyCache.iterate_read_queue this.cache in
         let filtered_var0 = AssemblyCache.enqueue_read_request cache'0 rqe in
         let (b0, cache'') = filtered_var0 in
         if b0 then (rres, (update_cache this cache'')) else ([], this)

  (** val run_write_requests :
      processor -> AssemblyCache.writequeueresult list * processor **)

  let run_write_requests this =
    let (wres, cache') = AssemblyCache.iterate_write_queue this.cache in
    (wres, (update_cache this cache'))

  (** val run_read_requests :
      processor -> AssemblyCache.readqueueresult list * processor **)

  let run_read_requests this =
    let (rres, cache') = AssemblyCache.iterate_read_queue this.cache in
    (rres, (update_cache this cache'))

  (** val get_keys : processor -> Store.KeyListStore.coq_R **)

  let get_keys this =
    this.cache.AssemblyCache.ackstore

  (** val get_fblocks : processor -> Store.FBlockListStore.coq_R **)

  let get_fblocks this =
    this.cache.AssemblyCache.acfbstore

  (** val close : processor -> processor **)

  let close this =
    let ac = AssemblyCache.close this.cache in update_cache this ac

  (** val block_sz : n **)

  let block_sz =
    Npos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO
      XH)))))))))))))))

  (** val rec_file_backup_inner :
      nat -> processor -> Filesupport.fileinformation -> n -> Cstdio.fptr ->
      processor **)

  let rec rec_file_backup_inner n_blocks this fi fpos fptr0 =
    match n_blocks with
    | O -> this
    | S n_blocks' ->
      if N.ltb fpos fi.Filesupport.fsize
      then let dsz = N.sub fi.Filesupport.fsize fpos in
           let sz = if N.ltb block_sz dsz then block_sz else dsz in
           let filtered_var = Cstdio.fread fptr0 sz in
           (match filtered_var with
            | Some p ->
              let (_, b) = p in
              let b' = Cstdio.BufferPlain.from_buffer b in
              let wqe = { AssemblyCache.qfhash =
                (Utilities.sha256 fi.Filesupport.fname);
                AssemblyCache.qfpos = fpos; AssemblyCache.qbuffer = b' }
              in
              let this' = backup_block this wqe in
              rec_file_backup_inner n_blocks' this' fi (N.add fpos sz) fptr0
            | None -> this)
      else this

  (** val open_file_backup :
      processor -> n -> Filesupport.fileinformation -> n -> processor **)

  let open_file_backup this n_blocks fi fpos =
    let filtered_var = Cstdio.fopen fi.Filesupport.fname Cstdio.read_mode in
    (match filtered_var with
     | Some fptr0 ->
       rec_file_backup_inner (N.to_nat n_blocks) this fi fpos fptr0
     | None -> this)

  (** val internal_restore_to :
      Cstdio.fptr -> AssemblyCache.readqueueresult list -> n **)

  let internal_restore_to fptr0 lrres =
    fold_left (fun acc rres ->
      let filtered_var =
        Cstdio.fseek fptr0 rres.AssemblyCache.readrequest.AssemblyCache.rqfpos
      in
      (match filtered_var with
       | Some fptr' ->
         let filtered_var0 =
           Cstdio.fwrite fptr'
             rres.AssemblyCache.readrequest.AssemblyCache.rqrlen
             (Cstdio.BufferPlain.to_buffer rres.AssemblyCache.rresult)
         in
         (match filtered_var0 with
          | Some n0 -> N.add n0 acc
          | None -> N0)
       | None -> N0)) lrres N0

  (** val restore_block_to :
      Cstdio.fptr -> AssemblyCache.assemblycache -> Assembly.blockinformation
      -> n * AssemblyCache.assemblycache **)

  let restore_block_to fptr0 ac block =
    let rreq = { AssemblyCache.rqaid = block.Assembly.blockaid;
      AssemblyCache.rqapos = block.Assembly.blockapos; AssemblyCache.rqrlen =
      block.Assembly.blocksize; AssemblyCache.rqfpos =
      block.Assembly.filepos }
    in
    let filtered_var = AssemblyCache.enqueue_read_request ac rreq in
    let (b, ac') = filtered_var in
    if b
    then (N0, ac')
    else let (lrres, ac'') = AssemblyCache.iterate_read_queue ac' in
         let n0 = internal_restore_to fptr0 lrres in
         let (_, ac''') = AssemblyCache.enqueue_read_request ac'' rreq in
         if N.ltb N0 n0 then (n0, ac''') else (N0, ac''')

  (** val restore_file_to :
      processor -> Cstdio.fptr -> Assembly.blockinformation list ->
      n * AssemblyCache.assemblycache **)

  let restore_file_to this fptr0 blocks =
    let filtered_var =
      fold_left (fun pat block ->
        let (acc, ac) = pat in
        let (n0, ac') = restore_block_to fptr0 ac block in
        ((N.add acc n0), ac')) blocks (N0, this.cache)
    in
    let (res, ac') = filtered_var in
    let (lrres, ac'') = AssemblyCache.iterate_read_queue ac' in
    let n0 = internal_restore_to fptr0 lrres in ((N.add n0 res), ac'')

  (** val file_backup :
      processor -> Filesystem.path -> Filesupport.fileinformation * processor **)

  let file_backup this fp =
    let fi = Filesupport.get_file_information (Filesystem.Path.to_string fp)
    in
    let n_blocks =
      N.add (Npos XH)
        (N.div
          (N.sub (N.add fi.Filesupport.fsize (N.div block_sz (Npos (XO XH))))
            (Npos XH)) block_sz)
    in
    let proc1 = open_file_backup this n_blocks fi N0 in
    let (_, proc2) = run_write_requests proc1 in (fi, proc2)

  (** val file_restore :
      processor -> Filesystem.path -> Filesystem.path ->
      Assembly.blockinformation list -> n * processor **)

  let file_restore this basep fp blocks =
    let targetp = Filesystem.Path.append basep fp in
    if Filesystem.Path.file_exists targetp
    then (N0, this)
    else let filtered_var =
           Cstdio.fopen (Filesystem.Path.to_string targetp)
             Cstdio.write_new_mode
         in
         (match filtered_var with
          | Some fptr0 ->
            let filtered_var0 = restore_file_to this fptr0 blocks in
            let (n0, ac') = filtered_var0 in
            let proc' = update_cache this ac' in
            let filtered_var1 = Cstdio.fclose fptr0 in
            (match filtered_var1 with
             | Some _ -> (n0, proc')
             | None -> (N0, proc'))
          | None -> (N0, this))

  (** val internal_directory_entries :
      Filesystem.path -> Filesystem.path list * Filesystem.path list **)

  let internal_directory_entries fp =
    Filesystem.list_directory fp ([], []) (fun de pat ->
      let (lfiles, ldirs) = pat in
      if Filesystem.Direntry.is_directory de
      then let defp = Filesystem.Direntry.as_path de in
           (lfiles, (defp :: ldirs))
      else if Filesystem.Direntry.is_regular_file de
           then let defp = Filesystem.Direntry.as_path de in
                ((defp :: lfiles), ldirs)
           else (lfiles, ldirs))

  (** val directory_backup :
      processor -> Filesystem.path -> Filesupport.fileinformation
      list * processor **)

  let directory_backup this fp =
    let filtered_var = internal_directory_entries fp in
    let (lfiles, _) = filtered_var in
    fold_left (fun pat filepath ->
      let (fis, proc0) = pat in
      let (fi, proc1) = file_backup proc0 filepath in ((fi :: fis), proc1))
      lfiles ([], this)

  (** val directory_backup_0 : processor -> Filesystem.path -> processor **)

  let directory_backup_0 this fp =
    let filtered_var = internal_directory_entries fp in
    let (lfiles, _) = filtered_var in
    fold_left (fun proc0 filepath ->
      let (_, proc1) = file_backup proc0 filepath in proc1) lfiles this

  (** val internal_recursive_backup :
      nat -> processor -> Filesupport.fileinformation list -> Filesystem.path
      -> Filesupport.fileinformation list * processor **)

  let rec internal_recursive_backup maxdepth this fis0 fp =
    match maxdepth with
    | O -> (fis0, this)
    | S depth ->
      Filesystem.list_directory fp (fis0, this) (fun de pat ->
        let (fis, proc) = pat in
        if Filesystem.Direntry.is_directory de
        then let defp = Filesystem.Direntry.as_path de in
             let filtered_var = internal_recursive_backup depth proc fis0 defp
             in
             let (fis', proc') = filtered_var in ((app fis' fis), proc')
        else if Filesystem.Direntry.is_regular_file de
             then let defp = Filesystem.Direntry.as_path de in
                  let filtered_var = file_backup proc defp in
                  let (fi, proc') = filtered_var in ((fi :: fis), proc')
             else (fis, proc))

  (** val recursive_backup :
      processor -> n -> Filesystem.path -> Filesupport.fileinformation
      list * processor **)

  let recursive_backup this maxdepth fp =
    if Filesystem.Path.is_directory fp
    then internal_recursive_backup (N.to_nat maxdepth) this [] fp
    else ([], this)

  (** val internal_recursive_backup_0 :
      nat -> processor -> Filesystem.path -> processor **)

  let rec internal_recursive_backup_0 maxdepth this fp =
    match maxdepth with
    | O -> this
    | S depth ->
      let filtered_var = internal_directory_entries fp in
      let (lfiles, ldirs) = filtered_var in
      let proc' =
        fold_left (fun proc0 filepath ->
          let (_, proc1) = file_backup proc0 filepath in proc1) lfiles this
      in
      fold_left (fun proc0 dirpath ->
        internal_recursive_backup_0 depth proc0 dirpath) ldirs proc'

  (** val recursive_backup_0 :
      processor -> n -> Filesystem.path -> processor **)

  let recursive_backup_0 this maxdepth fp =
    if Filesystem.Path.is_directory fp
    then internal_recursive_backup_0 (N.to_nat maxdepth) this fp
    else this
 end

module Version =
 struct
  (** val major : string **)

  let major =
    "0"

  (** val minor : string **)

  let minor =
    "9"

  (** val build : string **)

  let build =
    "9"

  (** val version : string **)

  let version =
    (^) major ((^) "." ((^) minor ((^) "." build)))
 end
