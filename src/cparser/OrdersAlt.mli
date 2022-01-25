open Datatypes

module type OrderedTypeOrig =
 OrderedType.OrderedType

module Update_OT :
 functor (O:OrderedTypeOrig) ->
 sig
  type t = O.t

  val eq_dec : t -> t -> bool

  val compare : O.t -> O.t -> comparison
 end
