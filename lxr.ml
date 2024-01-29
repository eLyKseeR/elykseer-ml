
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

module Buffer =
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

    let calc_checksum = fun b -> Elykseer_base.Buffer.sha256 b

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

    let calc_checksum = fun b -> Elykseer_base.Buffer.sha256 b

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

  let cpp_encrypt_buffer = fun b iv pw -> Helper.cpp_encrypt_buffer b iv pw

  (** val encrypt :
      BufferPlain.buffer_t -> string -> string -> BufferEncrypted.buffer_t **)

  let encrypt =
    cpp_encrypt_buffer

  (** val cpp_decrypt_buffer :
      BufferEncrypted.buffer_t -> string -> string -> BufferPlain.buffer_t **)

  let cpp_decrypt_buffer = fun b iv pw -> Helper.cpp_decrypt_buffer b iv pw

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
    type coq_B = Buffer.BufferPlain.buffer_t

    (** val buffer_len : coq_B -> n **)

    let buffer_len =
      Buffer.BufferPlain.buffer_len

    (** val calc_checksum : coq_B -> string **)

    let calc_checksum _ =
      "<>"

    (** val create :
        Configuration.configuration -> assemblyinformation * coq_B **)

    let create c =
      let chunks = c.Configuration.config_nchunks in
      let b =
        Buffer.BufferPlain.buffer_create
          (N.mul chunksize_N (Nchunks.to_N chunks))
      in
      let rb = Buffer.ranbuf128 () in
      let nb =
        Buffer.BufferPlain.copy_sz_pos rb N0 (Npos (XO (XO (XO (XO XH))))) b
          N0
      in
      ({ nchunks = chunks; aid = (Utilities.rnd256 c.Configuration.my_id);
      apos = nb }, b)
   end

  module AssemblyEncrypted =
   struct
    type coq_B = Buffer.BufferEncrypted.buffer_t

    (** val buffer_len : coq_B -> n **)

    let buffer_len =
      Buffer.BufferEncrypted.buffer_len

    (** val calc_checksum : coq_B -> string **)

    let calc_checksum =
      Buffer.BufferEncrypted.calc_checksum

    (** val create :
        Configuration.configuration -> assemblyinformation * coq_B **)

    let create c =
      let chunks = c.Configuration.config_nchunks in
      let b =
        Buffer.BufferEncrypted.buffer_create
          (N.mul chunksize_N (Nchunks.to_N chunks))
      in
      ({ nchunks = chunks; aid = (Utilities.rnd256 c.Configuration.my_id);
      apos = N0 }, b)
   end

  module AssemblyPlainFull =
   struct
    type coq_B = Buffer.BufferPlain.buffer_t

    (** val buffer_len : coq_B -> n **)

    let buffer_len =
      Buffer.BufferPlain.buffer_len

    (** val calc_checksum : coq_B -> string **)

    let calc_checksum =
      Buffer.BufferPlain.calc_checksum

    (** val create :
        Configuration.configuration -> assemblyinformation * coq_B **)

    let create c =
      let chunks = c.Configuration.config_nchunks in
      let sz = N.mul chunksize_N (Nchunks.to_N chunks) in
      let b = Buffer.BufferPlain.buffer_create sz in
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
      Buffer.BufferPlain.buffer_t -> n -> n -> AssemblyPlainWritable.coq_B ->
      n **)

  let assembly_add_content = 
    fun src sz_N pos_N tgt ->
      let sz = Conversion.n2i sz_N
      and pos = Conversion.n2i pos_N in
      Elykseer_base.Assembly.add_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt |> Conversion.i2n
   

  (** val backup :
      assemblyinformation -> AssemblyPlainWritable.coq_B -> n ->
      Buffer.BufferPlain.buffer_t -> assemblyinformation * blockinformation **)

  let backup a b fpos content =
    let apos_n = a.apos in
    let bsz = Buffer.BufferPlain.buffer_len content in
    let chksum = Buffer.BufferPlain.calc_checksum content in
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
      AssemblyPlainFull.coq_B -> Buffer.BufferPlain.buffer_t **)

  let id_buffer_t_from_full = fun b -> Helper.cpp_buffer_id b

  (** val id_assembly_enc_buffer_t_from_buf :
      Buffer.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B **)

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
    let benc = Buffer.encrypt (id_buffer_t_from_full b) ki.ivec ki.pkey in
    let b' = id_assembly_enc_buffer_t_from_buf benc in Some (a', b')

  (** val assembly_get_content :
      AssemblyPlainFull.coq_B -> n -> n -> Buffer.BufferPlain.buffer_t -> n **)

  let assembly_get_content = 
    fun src sz_N pos_N tgt ->
      let sz = Conversion.n2i sz_N
      and pos = Conversion.n2i pos_N in
      Elykseer_base.Assembly.get_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt |> Conversion.i2n
   

  (** val restore :
      AssemblyPlainFull.coq_B -> blockinformation ->
      Buffer.BufferPlain.buffer_t option **)

  let restore b bi =
    let bsz = bi.blocksize in
    let b' = Buffer.BufferPlain.buffer_create bsz in
    let nw = assembly_get_content b bsz bi.blockapos b' in
    if N.eqb nw bsz
    then let bcksum = Buffer.BufferPlain.calc_checksum b' in
         if (=) bcksum bi.bchecksum then Some b' else None
    else None

  (** val id_buffer_t_from_enc :
      AssemblyEncrypted.coq_B -> Buffer.BufferEncrypted.buffer_t **)

  let id_buffer_t_from_enc = fun b -> Helper.cpp_buffer_id b

  (** val id_assembly_plain_buffer_t_from_buf :
      Buffer.BufferPlain.buffer_t -> AssemblyPlainFull.coq_B **)

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
    let bdec = Buffer.decrypt (id_buffer_t_from_enc b) ki.ivec ki.pkey in
    let b' = id_assembly_plain_buffer_t_from_buf bdec in Some (a', b')

  (** val chunk_identifier :
      Configuration.configuration -> aid_t -> positive -> string **)

  let chunk_identifier =   
    fun config aid cid -> let s =
      (Configuration.my_id config) ^
      (string_of_int (Conversion.p2i cid)) ^
      aid in
      Elykseer_base.Hashing.sha256 s
   

  (** val chunk_identifier_path :
      Configuration.configuration -> aid_t -> positive -> string **)

  let chunk_identifier_path =   
    fun config aid cid -> let cident = chunk_identifier config aid cid in
      let subd = Helper.mk_cid_subdir cident in 
      (Configuration.path_chunks config ^ "/" ^ subd ^ "/" ^ cident ^ ".lxr")
   

  (** val ext_load_chunk_from_path :
      string -> Buffer.BufferEncrypted.buffer_t option **)

  let ext_load_chunk_from_path = 
    fun fp -> Helper.load_chunk_from_path (Mlcpp_filesystem.Filesystem.Path.from_string fp)
   

  (** val id_enc_from_buffer_t :
      Buffer.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B **)

  let id_enc_from_buffer_t = fun b -> Helper.cpp_buffer_id b

  (** val recall :
      Configuration.configuration -> assemblyinformation ->
      (assemblyinformation * AssemblyEncrypted.coq_B) option **)

  let recall c a =
    let cidlist = Utilities.make_list a.nchunks in
    let b =
      Buffer.BufferEncrypted.buffer_create
        (N.mul (Conversion.pos2N a.nchunks) chunksize_N)
    in
    let aid0 = a.aid in
    let blen = Buffer.BufferEncrypted.buffer_len b in
    let nread =
      fold_left (fun nread cid ->
        let cpath = chunk_identifier_path c aid0 cid in
        let filtered_var = ext_load_chunk_from_path cpath in
        (match filtered_var with
         | Some cb ->
           let apos0 =
             N.mul chunksize_N (N.sub (Conversion.pos2N cid) (Npos XH))
           in
           if N.leb (N.add apos0 chunksize_N) blen
           then N.add nread
                  (Buffer.BufferEncrypted.copy_sz_pos cb N0 chunksize_N b
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

  (** val ext_store_chunk_to_path :
      string -> n -> n -> Buffer.BufferEncrypted.buffer_t -> n **)

  let ext_store_chunk_to_path = 
    fun fp nsz npos b -> Conversion.i2n @@
      Helper.store_chunk_to_path (Mlcpp_filesystem.Filesystem.Path.from_string fp) (Conversion.n2i nsz) (Conversion.n2i npos) b
   

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
      N.add nwritten (ext_store_chunk_to_path cpath chunksize_N apos0 buf))
      (Utilities.make_list a.nchunks) N0
 end

module Store =
 struct
  type 'kVs store = { config : Configuration.configuration; entries : 'kVs }

  (** val config : 'a1 store -> Configuration.configuration **)

  let config s =
    s.config

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
      { config = c; entries = [] }

    (** val add : coq_K -> coq_V -> coq_R -> coq_R **)

    let add k v r =
      { config = r.config; entries = ((k, v) :: r.entries) }

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
      { config = c; entries = [] }

    (** val add : coq_K -> coq_V -> coq_R -> coq_R **)

    let add k v r =
      { config = r.config; entries = ((k, v) :: r.entries) }

    (** val find : coq_K -> coq_R -> coq_V option **)

    let find k r =
      rec_find k r.entries
   end
 end

module Environment =
 struct
  type 'aB environment = { cur_assembly : Assembly.assemblyinformation;
                           cur_buffer : 'aB;
                           config : Configuration.configuration;
                           fblocks : Store.FBlockListStore.coq_R;
                           keys : Store.KeyListStore.coq_R }

  (** val cur_assembly : 'a1 environment -> Assembly.assemblyinformation **)

  let cur_assembly e =
    e.cur_assembly

  (** val cur_buffer : 'a1 environment -> 'a1 **)

  let cur_buffer e =
    e.cur_buffer

  (** val config : 'a1 environment -> Configuration.configuration **)

  let config e =
    e.config

  (** val fblocks : 'a1 environment -> Store.FBlockListStore.coq_R **)

  let fblocks e =
    e.fblocks

  (** val keys : 'a1 environment -> Store.KeyListStore.coq_R **)

  let keys e =
    e.keys

  (** val cpp_mk_key256 : unit -> string **)

  let cpp_mk_key256 = fun () -> Helper.mk_key256 ()

  (** val cpp_mk_key128 : unit -> string **)

  let cpp_mk_key128 = fun () -> Helper.mk_key128 ()

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
      { cur_assembly = a; cur_buffer = b; config = c; fblocks =
      (Store.FBlockListStore.init c); keys = (Store.KeyListStore.init c) }

    (** val recreate_assembly : coq_AB environment -> coq_AB environment **)

    let recreate_assembly e =
      let (a, b) = Assembly.AssemblyPlainWritable.create e.config in
      { cur_assembly = a; cur_buffer = b; config = e.config; fblocks =
      e.fblocks; keys = e.keys }

    (** val env_add_file_block :
        string -> coq_AB environment -> Assembly.blockinformation -> coq_AB
        environment **)

    let env_add_file_block fname0 e bi =
      { cur_assembly = e.cur_assembly; cur_buffer = e.cur_buffer; config =
        e.config; fblocks = (Store.FBlockListStore.add fname0 bi e.fblocks);
        keys = e.keys }

    (** val env_add_aid_key :
        Assembly.aid_t -> coq_AB environment -> Assembly.keyinformation ->
        coq_AB environment **)

    let env_add_aid_key aid0 e ki =
      { cur_assembly = e.cur_assembly; cur_buffer = e.cur_buffer; config =
        e.config; fblocks = e.fblocks; keys =
        (Store.KeyListStore.add aid0 ki e.keys) }

    (** val key_for_aid :
        coq_AB environment -> Assembly.aid_t -> Assembly.keyinformation option **)

    let key_for_aid e aid0 =
      Store.KeyListStore.find aid0 e.keys

    (** val finalise_assembly : coq_AB environment -> coq_AB environment **)

    let finalise_assembly e0 =
      let a0 = e0.cur_assembly in
      let apos0 = a0.Assembly.apos in
      if N.ltb N0 apos0
      then let (a, b) = Assembly.finish a0 e0.cur_buffer in
           let ki = { Assembly.ivec = (cpp_mk_key128 ()); Assembly.pkey =
             (cpp_mk_key256 ()); Assembly.localid =
             e0.config.Configuration.my_id; Assembly.localnchunks =
             e0.config.Configuration.config_nchunks }
           in
           let e1 = env_add_aid_key a.Assembly.aid e0 ki in
           (match Assembly.encrypt a b ki with
            | Some p ->
              let (a', b') = p in
              let n0 = Assembly.extract e1.config a' b' in
              if N.eqb n0
                   (Assembly.assemblysize
                     e0.config.Configuration.config_nchunks)
              then e1
              else e0
            | None -> e0)
      else e0

    (** val finalise_and_recreate_assembly :
        coq_AB environment -> coq_AB environment **)

    let finalise_and_recreate_assembly e0 =
      let e1 = finalise_assembly e0 in recreate_assembly e1

    (** val backup :
        coq_AB environment -> string -> n -> Buffer.BufferPlain.buffer_t ->
        coq_AB environment **)

    let backup e0 fp fpos content =
      let afree =
        N.sub (Assembly.assemblysize e0.config.Configuration.config_nchunks)
          e0.cur_assembly.Assembly.apos
      in
      let blen = Buffer.BufferPlain.buffer_len content in
      let e1 =
        if N.ltb afree blen then finalise_and_recreate_assembly e0 else e0
      in
      let (a', bi) =
        Assembly.backup e1.cur_assembly e1.cur_buffer fpos content
      in
      { cur_assembly = a'; cur_buffer = e1.cur_buffer; config = e1.config;
      fblocks = (Store.FBlockListStore.add fp bi e1.fblocks); keys = e1.keys }
   end

  module EnvironmentReadable =
   struct
    type coq_AB = Assembly.AssemblyPlainFull.coq_B

    type coq_E = coq_AB environment

    (** val initial_environment : Configuration.configuration -> coq_E **)

    let initial_environment c =
      let (a, b) = Assembly.AssemblyPlainFull.create c in
      { cur_assembly = a; cur_buffer = b; config = c; fblocks =
      (Store.FBlockListStore.init c); keys = (Store.KeyListStore.init c) }

    (** val env_add_aid_key :
        Assembly.aid_t -> coq_AB environment -> Assembly.keyinformation ->
        coq_AB environment **)

    let env_add_aid_key aid0 e ki =
      { cur_assembly = e.cur_assembly; cur_buffer = e.cur_buffer; config =
        e.config; fblocks = e.fblocks; keys =
        (Store.KeyListStore.add aid0 ki e.keys) }

    (** val key_for_aid :
        coq_AB environment -> Assembly.aid_t -> Assembly.keyinformation option **)

    let key_for_aid e aid0 =
      Store.KeyListStore.find aid0 e.keys

    (** val restore_assembly :
        coq_AB environment -> Assembly.aid_t -> coq_AB environment option **)

    let restore_assembly e0 aid0 =
      match key_for_aid e0 aid0 with
      | Some k ->
        (match Assembly.recall e0.config { Assembly.nchunks =
                 e0.config.Configuration.config_nchunks; Assembly.aid = aid0;
                 Assembly.apos = N0 } with
         | Some p ->
           let (a1, b1) = p in
           (match Assembly.decrypt a1 b1 k with
            | Some p0 ->
              let (a2, b2) = p0 in
              Some { cur_assembly = a2; cur_buffer = b2; config = e0.config;
              fblocks = e0.fblocks; keys = e0.keys }
            | None -> None)
         | None -> None)
      | None -> None
   end
 end

module AssemblyCache =
 struct
  type readqueueentity = { qaid : Assembly.aid_t; qapos : n; qrlen : n }

  (** val qaid : readqueueentity -> Assembly.aid_t **)

  let qaid r =
    r.qaid

  (** val qapos : readqueueentity -> n **)

  let qapos r =
    r.qapos

  (** val qrlen : readqueueentity -> n **)

  let qrlen r =
    r.qrlen

  type readqueueresult = { readrequest : readqueueentity;
                           rresult : Buffer.BufferPlain.buffer_t }

  (** val readrequest : readqueueresult -> readqueueentity **)

  let readrequest r =
    r.readrequest

  (** val rresult : readqueueresult -> Buffer.BufferPlain.buffer_t **)

  let rresult r =
    r.rresult

  type writequeueentity = { qfhash : string; qfpos : n;
                            qbuffer : Buffer.BufferPlain.buffer_t }

  (** val qfhash : writequeueentity -> string **)

  let qfhash w =
    w.qfhash

  (** val qfpos : writequeueentity -> n **)

  let qfpos w =
    w.qfpos

  (** val qbuffer : writequeueentity -> Buffer.BufferPlain.buffer_t **)

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
                         acwriteq : writequeue; acreadq : readqueue }

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

  (** val prepare_assemblycache :
      Configuration.configuration -> positive -> assemblycache **)

  let prepare_assemblycache c size =
    { acenvs = []; acsize = (Coq_Pos.to_nat size); acwriteenv =
      (Environment.EnvironmentWritable.initial_environment c); acconfig = c;
      acwriteq = { wqueue = []; wqueuesz = qsize }; acreadq = { rqueue = [];
      rqueuesz = qsize } }

  (** val enqueue_write_request :
      assemblycache -> writequeueentity -> bool * assemblycache **)

  let enqueue_write_request ac req =
    let ln = length ac.acwriteq.wqueue in
    if N.leb (Conversion.pos2N qsize) (Conversion.nat2N ln)
    then (false, ac)
    else (true, { acenvs = ac.acenvs; acsize = ac.acsize; acwriteenv =
           ac.acwriteenv; acconfig = ac.acconfig; acwriteq = { wqueue =
           (app ac.acwriteq.wqueue (req :: [])); wqueuesz =
           ac.acwriteq.wqueuesz }; acreadq = ac.acreadq })

  (** val enqueue_read_request :
      assemblycache -> readqueueentity -> bool * assemblycache **)

  let enqueue_read_request ac req =
    let ln = length ac.acreadq.rqueue in
    if N.leb (Conversion.pos2N qsize) (Conversion.nat2N ln)
    then (false, ac)
    else (true, { acenvs = ac.acenvs; acsize = ac.acsize; acwriteenv =
           ac.acwriteenv; acconfig = ac.acconfig; acwriteq = ac.acwriteq;
           acreadq = { rqueue = (app ac.acreadq.rqueue (req :: []));
           rqueuesz = ac.acreadq.rqueuesz } })

  (** val try_restore_assembly :
      Configuration.configuration -> Assembly.aid_t ->
      Environment.EnvironmentReadable.coq_E option **)

  let try_restore_assembly config0 sel_aid =
    Environment.EnvironmentReadable.restore_assembly
      (Environment.EnvironmentReadable.initial_environment config0) sel_aid

  (** val set_envs :
      assemblycache -> Environment.EnvironmentReadable.coq_E list ->
      assemblycache **)

  let set_envs ac0 envs =
    { acenvs = envs; acsize = ac0.acsize; acwriteenv = ac0.acwriteenv;
      acconfig = ac0.acconfig; acwriteq = ac0.acwriteq; acreadq =
      ac0.acreadq }

  (** val ensure_assembly :
      assemblycache -> Assembly.aid_t ->
      (Environment.EnvironmentReadable.coq_E * assemblycache) option **)

  let ensure_assembly ac0 sel_aid =
    let filtered_var = ac0.acenvs in
    (match filtered_var with
     | [] ->
       let filtered_var0 = try_restore_assembly ac0.acconfig sel_aid in
       (match filtered_var0 with
        | Some env -> Some (env, (set_envs ac0 (env :: [])))
        | None -> None)
     | e1 :: r ->
       (match r with
        | [] ->
          if (=) e1.Environment.cur_assembly.Assembly.aid sel_aid
          then Some (e1, ac0)
          else let filtered_var0 = try_restore_assembly ac0.acconfig sel_aid
               in
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
                  let filtered_var0 =
                    try_restore_assembly ac0.acconfig sel_aid
                  in
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
      let aid0 = h.qaid in
      let filtered_var = ensure_assembly ac0 aid0 in
      (match filtered_var with
       | Some p ->
         let (_, ac1) = p in
         let buf = Buffer.BufferPlain.buffer_create h.qrlen in
         run_read_requests ac1 r ({ readrequest = h; rresult = buf } :: res)
       | None -> (res, ac0))

  (** val run_write_requests :
      assemblycache -> writequeueentity list -> writequeueresult list ->
      writequeueresult list * assemblycache **)

  let rec run_write_requests ac0 reqs res =
    match reqs with
    | [] -> (res, ac0)
    | h :: r ->
      let env =
        Environment.EnvironmentWritable.backup ac0.acwriteenv h.qfhash
          h.qfpos h.qbuffer
      in
      let ac1 = { acenvs = ac0.acenvs; acsize = ac0.acsize; acwriteenv = env;
        acconfig = ac0.acconfig; acwriteq = { wqueue = []; wqueuesz =
        ac0.acwriteq.wqueuesz }; acreadq = ac0.acreadq }
      in
      run_write_requests ac1 r ({ writerequest = h; wresult = { qaid =
        env.Environment.cur_assembly.Assembly.aid; qapos =
        env.Environment.cur_assembly.Assembly.apos; qrlen =
        (Buffer.BufferPlain.buffer_len h.qbuffer) } } :: res)

  (** val iterate_read_queue :
      assemblycache -> readqueueresult list * assemblycache **)

  let iterate_read_queue ac0 =
    let filtered_var = ac0.acreadq.rqueue in
    (match filtered_var with
     | [] -> ([], ac0)
     | h :: r ->
       let aid0 = h.qaid in
       let sel = filter (fun e -> (=) e.qaid aid0) r in
       let ac1 = { acenvs = ac0.acenvs; acsize = ac0.acsize; acwriteenv =
         ac0.acwriteenv; acconfig = ac0.acconfig; acwriteq = ac0.acwriteq;
         acreadq = { rqueue = (filter (fun e -> negb ((=) e.qaid aid0)) r);
         rqueuesz = ac0.acreadq.rqueuesz } }
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
         wqueuesz = ac0.acwriteq.wqueuesz }; acreadq = ac0.acreadq }
       in
       run_write_requests ac1 (h :: r) [])

  (** val flush : assemblycache -> assemblycache **)

  let flush ac0 =
    let env =
      Environment.EnvironmentWritable.finalise_and_recreate_assembly
        ac0.acwriteenv
    in
    { acenvs = []; acsize = ac0.acsize; acwriteenv = env; acconfig =
    ac0.acconfig; acwriteq = ac0.acwriteq; acreadq = ac0.acreadq }

  (** val close : assemblycache -> assemblycache **)

  let close ac0 =
    let env = Environment.EnvironmentWritable.finalise_assembly ac0.acwriteenv
    in
    { acenvs = []; acsize = ac0.acsize; acwriteenv = env; acconfig =
    ac0.acconfig; acwriteq = ac0.acwriteq; acreadq = ac0.acreadq }
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
    "7"

  (** val version : string **)

  let version =
    (^) major ((^) "." ((^) minor ((^) "." build)))
 end
