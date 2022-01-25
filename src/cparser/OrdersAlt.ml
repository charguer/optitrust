open Datatypes

module type OrderedTypeOrig =
 OrderedType.OrderedType

module Update_OT =
 functor (O:OrderedTypeOrig) ->
 struct
  type t = O.t

  (** val eq_dec : t -> t -> bool **)

  let eq_dec =
    O.eq_dec

  (** val compare : O.t -> O.t -> comparison **)

  let compare x y =
    match O.compare x y with
    | OrderedType.LT -> Lt
    | OrderedType.EQ -> Eq
    | OrderedType.GT -> Gt
 end
