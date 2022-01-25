open BinInt
open BinNums
open BinPos
open Coqlib
open Datatypes
open Maps

module OrderedPositive =
 struct
  type t = positive

  (** val compare : t -> t -> positive OrderedType.coq_Compare **)

  let compare x y =
    let c = Pos.compare x y in
    (match c with
     | Eq -> OrderedType.EQ
     | Lt -> OrderedType.LT
     | Gt -> OrderedType.GT)

  (** val eq_dec : t -> t -> bool **)

  let eq_dec =
    peq
 end

module OrderedZ =
 struct
  type t = coq_Z

  (** val compare : t -> t -> coq_Z OrderedType.coq_Compare **)

  let compare x y =
    let c = Z.compare x y in
    (match c with
     | Eq -> OrderedType.EQ
     | Lt -> OrderedType.LT
     | Gt -> OrderedType.GT)
 end

module OrderedIndexed =
 functor (A:INDEXED_TYPE) ->
 struct
  type t = A.t

  (** val compare : t -> t -> t OrderedType.coq_Compare **)

  let compare x y =
    match OrderedPositive.compare (A.index x) (A.index y) with
    | OrderedType.LT -> OrderedType.LT
    | OrderedType.EQ -> OrderedType.EQ
    | OrderedType.GT -> OrderedType.GT

  (** val eq_dec : t -> t -> bool **)

  let eq_dec x y =
    peq (A.index x) (A.index y)
 end
