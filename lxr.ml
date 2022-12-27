
type nat =
| O
| S of nat

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

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

(** val seq : nat -> nat -> nat list **)

let rec seq start = function
| O -> []
| S len0 -> start :: (seq (S start) len0)

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
 end

module Configuration =
 struct
  type configuration = { config_nchunks : Nchunks.t; path_chunks : string;
                         path_db : string; my_id : n }

  (** val config_nchunks : configuration -> Nchunks.t **)

  let config_nchunks c =
    c.config_nchunks

  (** val path_chunks : configuration -> string **)

  let path_chunks c =
    c.path_chunks

  (** val path_db : configuration -> string **)

  let path_db c =
    c.path_db

  (** val my_id : configuration -> n **)

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
   

  (** val rnd256 : n -> string **)

  let rnd256 = 
   function
   x -> Elykseer_crypto.Random.with_rng (fun rng -> Elykseer_crypto.Random.random32 rng) |> string_of_int |>
     String.cat (Conversion.n2i x |> string_of_int) |>
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

  type keyinformation = { ivec : string; pkey : string; localid : n;
                          localnchunks : positive }

  (** val ivec : keyinformation -> string **)

  let ivec k =
    k.ivec

  (** val pkey : keyinformation -> string **)

  let pkey k =
    k.pkey

  (** val localid : keyinformation -> n **)

  let localid k =
    k.localid

  (** val localnchunks : keyinformation -> positive **)

  let localnchunks k =
    k.localnchunks

  module type ASS =
   sig
    type coq_H = assemblyinformation

    type coq_B

    val create : Configuration.configuration -> coq_H * coq_B

    val buffer_len : coq_B -> n

    val calc_checksum : coq_B -> string
   end

  module AssemblyPlainWritable =
   struct
    type coq_H = assemblyinformation

    type coq_B = Buffer.BufferPlain.buffer_t

    (** val buffer_len : coq_B -> n **)

    let buffer_len =
      Buffer.BufferPlain.buffer_len

    (** val calc_checksum : coq_B -> string **)

    let calc_checksum _ =
      "<>"

    (** val create : Configuration.configuration -> coq_H * coq_B **)

    let create c =
      let chunks = c.Configuration.config_nchunks in
      let b =
        Buffer.BufferPlain.buffer_create
          (N.mul chunksize_N (Nchunks.to_N chunks))
      in
      ({ nchunks = chunks; aid = (Utilities.rnd256 c.Configuration.my_id);
      apos = N0 }, b)
   end

  module AssemblyEncrypted =
   struct
    type coq_H = assemblyinformation

    type coq_B = Buffer.BufferEncrypted.buffer_t

    (** val buffer_len : coq_B -> n **)

    let buffer_len =
      Buffer.BufferEncrypted.buffer_len

    (** val calc_checksum : coq_B -> string **)

    let calc_checksum =
      Buffer.BufferEncrypted.calc_checksum

    (** val create : Configuration.configuration -> coq_H * coq_B **)

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
    type coq_H = assemblyinformation

    type coq_B = Buffer.BufferPlain.buffer_t

    (** val buffer_len : coq_B -> n **)

    let buffer_len =
      Buffer.BufferPlain.buffer_len

    (** val calc_checksum : coq_B -> string **)

    let calc_checksum =
      Buffer.BufferPlain.calc_checksum

    (** val create :
        Configuration.configuration ->
        assemblyinformation * Buffer.BufferPlain.buffer_t **)

    let create c =
      let chunks = c.Configuration.config_nchunks in
      let sz = N.mul chunksize_N (Nchunks.to_N chunks) in
      let b = Buffer.BufferPlain.buffer_create sz in
      ({ nchunks = chunks; aid = (Utilities.rnd256 c.Configuration.my_id);
      apos = sz }, b)
   end

  (** val id_buffer_t_from_enc :
      AssemblyEncrypted.coq_B -> Buffer.BufferEncrypted.buffer_t **)

  let id_buffer_t_from_enc = fun b -> Helper.cpp_buffer_id b

  (** val id_enc_from_buffer_t :
      Buffer.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B **)

  let id_enc_from_buffer_t = fun b -> Helper.cpp_buffer_id b

  (** val id_assembly_plain_buffer_t_from_buf :
      Buffer.BufferPlain.buffer_t -> AssemblyPlainWritable.coq_B **)

  let id_assembly_plain_buffer_t_from_buf = fun b -> Helper.cpp_buffer_id b

  (** val decrypt :
      AssemblyEncrypted.coq_H -> AssemblyEncrypted.coq_B -> keyinformation ->
      (AssemblyPlainWritable.coq_H * AssemblyPlainWritable.coq_B) option **)

  let decrypt a b ki =
    let a' = { nchunks = a.nchunks; aid = a.aid; apos = N0 } in
    let bdec = Buffer.decrypt (id_buffer_t_from_enc b) ki.ivec ki.pkey in
    let b' = id_assembly_plain_buffer_t_from_buf bdec in Some (a', b')

  (** val chunk_identifier :
      Configuration.configuration -> aid_t -> positive -> string **)

  let chunk_identifier =   
    fun config aid cid -> let s =
      (string_of_int (Conversion.n2i (Configuration.my_id config))) ^
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
   

  (** val recall :
      Configuration.configuration -> AssemblyEncrypted.coq_H ->
      (AssemblyEncrypted.coq_H * AssemblyEncrypted.coq_B) option **)

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
    let a' = { nchunks = a.nchunks; aid = aid0; apos = nread } in
    let b' = id_enc_from_buffer_t b in Some (a', b')

  (** val ext_store_chunk_to_path :
      string -> n -> n -> Buffer.BufferEncrypted.buffer_t -> n **)

  let ext_store_chunk_to_path = 
    fun fp nsz npos b -> Conversion.i2n @@
      Helper.store_chunk_to_path (Mlcpp_filesystem.Filesystem.Path.from_string fp) (Conversion.n2i nsz) (Conversion.n2i npos) b
   

  (** val extract :
      Configuration.configuration -> AssemblyEncrypted.coq_H ->
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

  (** val id_buffer_t_from_full :
      AssemblyPlainFull.coq_B -> Buffer.BufferPlain.buffer_t **)

  let id_buffer_t_from_full = fun b -> Helper.cpp_buffer_id b

  (** val id_buffer_t_from_writable :
      AssemblyPlainWritable.coq_B -> Buffer.BufferPlain.buffer_t **)

  let id_buffer_t_from_writable = fun b -> Helper.cpp_buffer_id b

  (** val id_assembly_enc_buffer_t_from_buf :
      Buffer.BufferEncrypted.buffer_t -> AssemblyEncrypted.coq_B **)

  let id_assembly_enc_buffer_t_from_buf = fun b -> Helper.cpp_buffer_id b

  (** val id_assembly_full_buffer_from_writable :
      AssemblyPlainWritable.coq_B -> AssemblyPlainFull.coq_B **)

  let id_assembly_full_buffer_from_writable = fun b -> Helper.cpp_buffer_id b

  (** val finish :
      AssemblyPlainWritable.coq_H -> AssemblyPlainWritable.coq_B ->
      AssemblyPlainFull.coq_H * AssemblyPlainFull.coq_B **)

  let finish a b =
    ({ nchunks = a.nchunks; aid = a.aid; apos = a.apos },
      (id_assembly_full_buffer_from_writable b))

  (** val encrypt :
      AssemblyPlainFull.coq_H -> AssemblyPlainFull.coq_B -> keyinformation ->
      (AssemblyEncrypted.coq_H * AssemblyEncrypted.coq_B) option **)

  let encrypt a b ki =
    let a' = { nchunks = a.nchunks; aid = a.aid; apos =
      (assemblysize a.nchunks) }
    in
    let benc = Buffer.encrypt (id_buffer_t_from_full b) ki.ivec ki.pkey in
    let b' = id_assembly_enc_buffer_t_from_buf benc in Some (a', b')

  type blockinformation = { blockid : positive; bchecksum : string;
                            blocksize : n; filepos : n; blockaid : string;
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

  (** val blockaid : blockinformation -> string **)

  let blockaid b =
    b.blockaid

  (** val blockapos : blockinformation -> n **)

  let blockapos b =
    b.blockapos

  (** val assembly_add_content :
      Buffer.BufferPlain.buffer_t -> n -> n -> AssemblyPlainWritable.coq_B ->
      n **)

  let assembly_add_content = 
    fun src sz_N pos_N tgt ->
      let sz = Conversion.n2i sz_N
      and pos = Conversion.n2i pos_N in
      Elykseer_base.Assembly.add_content ~src:src ~sz:sz ~pos:pos ~tgt:tgt |> Conversion.i2n
   

  (** val backup :
      AssemblyPlainWritable.coq_H -> AssemblyPlainWritable.coq_B -> n ->
      Buffer.BufferPlain.buffer_t ->
      AssemblyPlainWritable.coq_H * blockinformation **)

  let backup a b fpos content =
    let apos_n = a.apos in
    let bsz = Buffer.BufferPlain.buffer_len content in
    let chksum = Buffer.BufferPlain.calc_checksum content in
    let nwritten = assembly_add_content content bsz apos_n b in
    let bi = { blockid = XH; bchecksum = chksum; blocksize = nwritten;
      filepos = fpos; blockaid = a.aid; blockapos = apos_n }
    in
    let a' = { nchunks = a.nchunks; aid = a.aid; apos =
      (N.add apos_n nwritten) }
    in
    (a', bi)

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
 end

module Filetypes =
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
 end

module Environment =
 struct
  type environment = { cur_assembly : Assembly.AssemblyPlainWritable.coq_H;
                       cur_buffer : Assembly.AssemblyPlainWritable.coq_B;
                       config : Configuration.configuration;
                       fblocks : (string * Assembly.blockinformation) list;
                       keys : (string * Assembly.keyinformation) list }

  (** val cur_assembly :
      environment -> Assembly.AssemblyPlainWritable.coq_H **)

  let cur_assembly e =
    e.cur_assembly

  (** val cur_buffer : environment -> Assembly.AssemblyPlainWritable.coq_B **)

  let cur_buffer e =
    e.cur_buffer

  (** val config : environment -> Configuration.configuration **)

  let config e =
    e.config

  (** val fblocks :
      environment -> (string * Assembly.blockinformation) list **)

  let fblocks e =
    e.fblocks

  (** val keys : environment -> (string * Assembly.keyinformation) list **)

  let keys e =
    e.keys

  (** val initial_environment : Configuration.configuration -> environment **)

  let initial_environment c =
    let (a, b) = Assembly.AssemblyPlainWritable.create c in
    { cur_assembly = a; cur_buffer = b; config = c; fblocks = []; keys = [] }

  (** val recreate_assembly : environment -> environment **)

  let recreate_assembly e =
    let (a, b) = Assembly.AssemblyPlainWritable.create e.config in
    { cur_assembly = a; cur_buffer = b; config = e.config; fblocks =
    e.fblocks; keys = e.keys }

  (** val env_add_file_block :
      string -> environment -> Assembly.blockinformation -> environment **)

  let env_add_file_block fname0 e bi =
    { cur_assembly = e.cur_assembly; cur_buffer = e.cur_buffer; config =
      e.config; fblocks = ((fname0, bi) :: e.fblocks); keys = e.keys }

  (** val env_add_aid_key :
      string -> environment -> Assembly.keyinformation -> environment **)

  let env_add_aid_key aid0 e ki =
    { cur_assembly = e.cur_assembly; cur_buffer = e.cur_buffer; config =
      e.config; fblocks = e.fblocks; keys = ((aid0, ki) :: e.keys) }

  (** val backup :
      environment -> string -> n -> Buffer.BufferPlain.buffer_t -> environment **)

  let backup e0 fp fpos content =
    let (a', bi) = Assembly.backup e0.cur_assembly e0.cur_buffer fpos content
    in
    { cur_assembly = a'; cur_buffer = e0.cur_buffer; config = e0.config;
    fblocks = ((fp, bi) :: e0.fblocks); keys = e0.keys }
 end

module Filesupport =
 struct
  (** val get_file_information :
      Filetypes.filename -> Filetypes.fileinformation **)

  let get_file_information =   
    fun fn ->
        { Filetypes.fname = fn;
          Filetypes.fsize = Conversion.i2n (Elykseer_base.Fsutils.fsize fn);
          Filetypes.fowner = string_of_int (Elykseer_base.Fsutils.fowner fn);
          Filetypes.fpermissions = Conversion.i2n (Elykseer_base.Fsutils.fperm fn);
          Filetypes.fmodified = Elykseer_base.Fsutils.fmod fn;
          Filetypes.fchecksum = Elykseer_base.Fsutils.fchksum fn }
   
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

  type fileblockinformation = { fbifi : Filetypes.fileinformation;
                                fbifblocks : fileblock list }

  (** val fbifi : fileblockinformation -> Filetypes.fileinformation **)

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
    Npos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO
      XH)))))))))))))))))

  (** val analyse_file :
      positive -> n -> positive -> string ->
      (positive * n) * fileblockinformation **)

  let analyse_file nchunks0 afree_p anum_p fn =
    let fi = Filesupport.get_file_information fn in
    let nblocks = N.div fi.Filetypes.fsize max_block_size in
    let fuel =
      N.to_nat
        (N.add (N.add nblocks (N.div nblocks (Conversion.pos2N nchunks0)))
          (Npos XH))
    in
    let (afbs, ares) =
      prepare_blocks nchunks0 max_block_size fuel anum_p afree_p [] N0
        fi.Filetypes.fsize
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
    "2"

  (** val version : string **)

  let version =
    (^) major ((^) "." ((^) minor ((^) "." build)))
 end
