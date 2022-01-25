open Datatypes

type __ = Obj.t

module type MAP =
 sig
  type elt

  val elt_eq : elt -> elt -> bool

  type 'x t

  val empty : 'a1 t

  val get : elt -> 'a1 t -> 'a1 option

  val set : elt -> 'a1 -> 'a1 t -> 'a1 t
 end

module type UNIONFIND =
 sig
  type elt

  val elt_eq : elt -> elt -> bool

  type t

  val repr : t -> elt -> elt

  val empty : t

  val find : t -> elt -> elt * t

  val union : t -> elt -> elt -> t

  val merge : t -> elt -> elt -> t

  val pathlen : t -> elt -> nat
 end

module UF :
 functor (M:MAP) ->
 UNIONFIND with type elt = M.elt
