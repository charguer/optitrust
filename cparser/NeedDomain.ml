open AST
open BinInt
open BinNums
open Datatypes
open Integers
open IntvSets
open Lattice
open Maps
open Registers
open ValueDomain

type nval =
| Nothing
| I of Int.int
| All

(** val eq_nval : nval -> nval -> bool **)

let eq_nval x y =
  match x with
  | Nothing -> (match y with
                | Nothing -> true
                | _ -> false)
  | I x0 -> (match y with
             | I m0 -> Int.eq_dec x0 m0
             | _ -> false)
  | All -> (match y with
            | All -> true
            | _ -> false)

(** val nlub : nval -> nval -> nval **)

let nlub x y =
  match x with
  | Nothing -> y
  | I m1 ->
    (match y with
     | Nothing -> x
     | I m2 -> I (Int.coq_or m1 m2)
     | All -> All)
  | All -> (match y with
            | Nothing -> x
            | _ -> All)

(** val complete_mask : Int.int -> Int.int **)

let complete_mask m =
  Int.zero_ext (Int.size m) Int.mone

(** val andimm : nval -> Int.int -> nval **)

let andimm x n =
  match x with
  | Nothing -> Nothing
  | I m -> I (Int.coq_and m n)
  | All -> I n

(** val orimm : nval -> Int.int -> nval **)

let orimm x n =
  match x with
  | Nothing -> Nothing
  | I m -> I (Int.coq_and m (Int.not n))
  | All -> I (Int.not n)

(** val bitwise : nval -> nval **)

let bitwise x =
  x

(** val shlimm : nval -> Int.int -> nval **)

let shlimm x n =
  match x with
  | Nothing -> Nothing
  | I m -> I (Int.shru m n)
  | All -> I (Int.shru Int.mone n)

(** val shruimm : nval -> Int.int -> nval **)

let shruimm x n =
  match x with
  | Nothing -> Nothing
  | I m -> I (Int.shl m n)
  | All -> I (Int.shl Int.mone n)

(** val shrimm : nval -> Int.int -> nval **)

let shrimm x n =
  match x with
  | Nothing -> Nothing
  | I m ->
    I
      (let m' = Int.shl m n in
       if Int.eq_dec (Int.shru m' n) m
       then m'
       else Int.coq_or m' (Int.repr Int.min_signed))
  | All -> I (Int.coq_or (Int.shl Int.mone n) (Int.repr Int.min_signed))

(** val ror : nval -> Int.int -> nval **)

let ror x amount =
  match x with
  | I m -> I (Int.rol m amount)
  | x0 -> x0

(** val modarith : nval -> nval **)

let modarith = function
| I m -> I (complete_mask m)
| x0 -> x0

(** val zero_ext : coq_Z -> nval -> nval **)

let zero_ext n = function
| Nothing -> Nothing
| I m -> I (Int.zero_ext n m)
| All -> I (Int.zero_ext n Int.mone)

(** val sign_ext : coq_Z -> nval -> nval **)

let sign_ext n = function
| Nothing -> Nothing
| I m ->
  I
    (Int.coq_or (Int.zero_ext n m)
      (Int.shl Int.one (Int.repr (Z.sub n (Zpos Coq_xH)))))
| All -> I (Int.zero_ext n Int.mone)

(** val store_argument : memory_chunk -> nval **)

let store_argument = function
| Mint8signed ->
  I
    (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      Coq_xH)))))))))
| Mint8unsigned ->
  I
    (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      Coq_xH)))))))))
| Mint16signed ->
  I
    (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      Coq_xH)))))))))))))))))
| Mint16unsigned ->
  I
    (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      Coq_xH)))))))))))))))))
| _ -> All

(** val maskzero : Int.int -> nval **)

let maskzero n =
  I n

(** val default : nval -> nval **)

let default = function
| Nothing -> Nothing
| _ -> All

(** val andimm_redundant : nval -> Int.int -> bool **)

let andimm_redundant x n =
  match x with
  | Nothing -> true
  | I m -> (fun x -> x) (Int.eq_dec (Int.coq_and m (Int.not n)) Int.zero)
  | All -> false

(** val orimm_redundant : nval -> Int.int -> bool **)

let orimm_redundant x n =
  match x with
  | Nothing -> true
  | I m -> (fun x -> x) (Int.eq_dec (Int.coq_and m n) Int.zero)
  | All -> false

(** val zero_ext_redundant : coq_Z -> nval -> bool **)

let zero_ext_redundant n = function
| Nothing -> true
| I m -> (fun x -> x) (Int.eq_dec (Int.zero_ext n m) m)
| All -> false

(** val sign_ext_redundant : coq_Z -> nval -> bool **)

let sign_ext_redundant n = function
| Nothing -> true
| I m -> (fun x -> x) (Int.eq_dec (Int.zero_ext n m) m)
| All -> false

module NVal =
 struct
  type t = nval

  (** val beq : t -> t -> bool **)

  let beq x y =
    (fun x -> x) (eq_nval x y)

  (** val bot : t **)

  let bot =
    Nothing

  (** val lub : nval -> nval -> nval **)

  let lub =
    nlub
 end

module NE = LPMap1(NVal)

type nenv = NE.t

(** val nreg : nenv -> reg -> NVal.t **)

let nreg ne r =
  NE.get r ne

type nmem =
| NMemDead
| NMem of ISet.t * ISet.t PTree.t

(** val nmem_all : nmem **)

let nmem_all =
  NMem (ISet.empty, PTree.empty)

(** val nmem_add : nmem -> aptr -> coq_Z -> nmem **)

let nmem_add nm p sz =
  match nm with
  | NMemDead -> nmem_all
  | NMem (stk, gl) ->
    (match p with
     | Gl (id, ofs) ->
       (match PTree.get id gl with
        | Some iv ->
          NMem (stk,
            (PTree.set id
              (ISet.remove (Ptrofs.unsigned ofs)
                (Z.add (Ptrofs.unsigned ofs) sz) iv) gl))
        | None -> nm)
     | Glo id -> NMem (stk, (PTree.remove id gl))
     | Stk ofs ->
       NMem
         ((ISet.remove (Ptrofs.unsigned ofs) (Z.add (Ptrofs.unsigned ofs) sz)
            stk), gl)
     | Stack -> NMem (ISet.empty, gl)
     | _ -> nmem_all)

(** val nmem_remove : nmem -> aptr -> coq_Z -> nmem **)

let nmem_remove nm p sz =
  match nm with
  | NMemDead -> NMemDead
  | NMem (stk, gl) ->
    (match p with
     | Gl (id, ofs) ->
       let iv' =
         match PTree.get id gl with
         | Some iv ->
           ISet.add (Ptrofs.unsigned ofs) (Z.add (Ptrofs.unsigned ofs) sz) iv
         | None ->
           ISet.interval (Ptrofs.unsigned ofs)
             (Z.add (Ptrofs.unsigned ofs) sz)
       in
       NMem (stk, (PTree.set id iv' gl))
     | Stk ofs ->
       NMem
         ((ISet.add (Ptrofs.unsigned ofs) (Z.add (Ptrofs.unsigned ofs) sz)
            stk), gl)
     | _ -> nm)

(** val nmem_contains : nmem -> aptr -> coq_Z -> bool **)

let nmem_contains nm p sz =
  match nm with
  | NMemDead -> false
  | NMem (stk, gl) ->
    (match p with
     | Gl (id, ofs) ->
       (match PTree.get id gl with
        | Some iv ->
          negb
            (ISet.contains (Ptrofs.unsigned ofs)
              (Z.add (Ptrofs.unsigned ofs) sz) iv)
        | None -> true)
     | Stk ofs ->
       negb
         (ISet.contains (Ptrofs.unsigned ofs)
           (Z.add (Ptrofs.unsigned ofs) sz) stk)
     | _ -> true)

(** val nmem_dead_stack : coq_Z -> nmem **)

let nmem_dead_stack sz =
  NMem ((ISet.interval Z0 sz), PTree.empty)

(** val nmem_lub : nmem -> nmem -> nmem **)

let nmem_lub nm1 nm2 =
  match nm1 with
  | NMemDead -> nm2
  | NMem (stk1, gl1) ->
    (match nm2 with
     | NMemDead -> nm1
     | NMem (stk2, gl2) ->
       NMem ((ISet.inter stk1 stk2),
         (PTree.combine (fun o1 o2 ->
           match o1 with
           | Some iv1 ->
             (match o2 with
              | Some iv2 -> Some (ISet.inter iv1 iv2)
              | None -> None)
           | None -> None) gl1 gl2)))

(** val nmem_beq : nmem -> nmem -> bool **)

let nmem_beq nm1 nm2 =
  match nm1 with
  | NMemDead -> (match nm2 with
                 | NMemDead -> true
                 | NMem (_, _) -> false)
  | NMem (stk1, gl1) ->
    (match nm2 with
     | NMemDead -> false
     | NMem (stk2, gl2) ->
       (&&) (ISet.beq stk1 stk2) (PTree.beq ISet.beq gl1 gl2))

module NA =
 struct
  type t = nenv * nmem

  (** val beq : t -> t -> bool **)

  let beq x y =
    (&&) (NE.beq (fst x) (fst y)) (nmem_beq (snd x) (snd y))

  (** val bot : t **)

  let bot =
    (NE.bot, NMemDead)

  (** val lub : t -> t -> t **)

  let lub x y =
    ((NE.lub (fst x) (fst y)), (nmem_lub (snd x) (snd y)))
 end
