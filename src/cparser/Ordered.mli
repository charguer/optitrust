open BinInt
open BinNums
open BinPos
open Coqlib
open Datatypes
open Maps

module OrderedPositive :
 sig
  type t = positive

  val compare : t -> t -> positive OrderedType.coq_Compare

  val eq_dec : t -> t -> bool
 end

module OrderedZ :
 sig
  type t = coq_Z

  val compare : t -> t -> coq_Z OrderedType.coq_Compare
 end

module OrderedIndexed :
 functor (A:INDEXED_TYPE) ->
 sig
  type t = A.t

  val compare : t -> t -> t OrderedType.coq_Compare

  val eq_dec : t -> t -> bool
 end
