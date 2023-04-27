open Datatypes
open Int0
open MSetAVL

module Make =
 functor (X:OrderedType.OrderedType) ->
 struct
  module X' =
   struct
    type t = X.t

    (** val eq_dec : t -> t -> bool **)

    let eq_dec =
      X.eq_dec

    (** val compare : X.t -> X.t -> comparison **)

    let compare x y =
      match X.compare x y with
      | OrderedType.LT -> Lt
      | OrderedType.EQ -> Eq
      | OrderedType.GT -> Gt
   end

  module MSet = IntMake(Z_as_Int)(X')

  type elt = X.t

  type t = MSet.t

  (** val empty : t **)

  let empty =
    MSet.empty

  (** val is_empty : t -> bool **)

  let is_empty =
    MSet.is_empty

  (** val mem : elt -> t -> bool **)

  let mem =
    MSet.mem

  (** val add : elt -> t -> t **)

  let add =
    MSet.add

  (** val singleton : elt -> t **)

  let singleton =
    MSet.singleton

  (** val remove : elt -> t -> t **)

  let remove =
    MSet.remove

  (** val union : t -> t -> t **)

  let union =
    MSet.union

  (** val inter : t -> t -> t **)

  let inter =
    MSet.inter

  (** val diff : t -> t -> t **)

  let diff =
    MSet.diff

  (** val eq_dec : t -> t -> bool **)

  let eq_dec =
    MSet.eq_dec

  (** val equal : t -> t -> bool **)

  let equal =
    MSet.equal

  (** val subset : t -> t -> bool **)

  let subset =
    MSet.subset

  (** val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1 **)

  let fold =
    MSet.fold

  (** val for_all : (elt -> bool) -> t -> bool **)

  let for_all =
    MSet.for_all

  (** val exists_ : (elt -> bool) -> t -> bool **)

  let exists_ =
    MSet.exists_

  (** val filter : (elt -> bool) -> t -> t **)

  let filter =
    MSet.filter

  (** val partition : (elt -> bool) -> t -> t * t **)

  let partition =
    MSet.partition

  (** val cardinal : t -> nat **)

  let cardinal =
    MSet.cardinal

  (** val elements : t -> elt list **)

  let elements =
    MSet.elements

  (** val choose : t -> elt option **)

  let choose =
    MSet.choose

  module MF =
   struct
    (** val eqb : X.t -> X.t -> bool **)

    let eqb x y =
      if MSet.E.eq_dec x y then true else false
   end

  (** val min_elt : t -> elt option **)

  let min_elt =
    MSet.min_elt

  (** val max_elt : t -> elt option **)

  let max_elt =
    MSet.max_elt

  (** val compare : t -> t -> t OrderedType.coq_Compare **)

  let compare s s' =
    let c = coq_CompSpec2Type s s' (MSet.compare s s') in
    (match c with
     | CompEqT -> OrderedType.EQ
     | CompLtT -> OrderedType.LT
     | CompGtT -> OrderedType.GT)

  module E =
   struct
    type t = X.t

    (** val compare : t -> t -> t OrderedType.coq_Compare **)

    let compare =
      X.compare

    (** val eq_dec : t -> t -> bool **)

    let eq_dec =
      X.eq_dec
   end

  module Raw = MSet.Raw

  (** val raw_mem_between :
      (elt -> bool) -> (elt -> bool) -> Raw.tree -> bool **)

  let rec raw_mem_between above_low_bound below_high_bound = function
  | Raw.Leaf -> false
  | Raw.Node (_, l, x, r) ->
    if above_low_bound x
    then if below_high_bound x
         then true
         else raw_mem_between above_low_bound below_high_bound l
    else raw_mem_between above_low_bound below_high_bound r

  (** val mem_between : (elt -> bool) -> (elt -> bool) -> t -> bool **)

  let mem_between above_low_bound below_high_bound m =
    raw_mem_between above_low_bound below_high_bound (MSet.this m)

  (** val raw_elements_between :
      (elt -> bool) -> (elt -> bool) -> Raw.tree -> Raw.tree **)

  let rec raw_elements_between above_low_bound below_high_bound = function
  | Raw.Leaf -> Raw.Leaf
  | Raw.Node (_, l, x, r) ->
    if above_low_bound x
    then if below_high_bound x
         then Raw.join
                (raw_elements_between above_low_bound below_high_bound l) x
                (raw_elements_between above_low_bound below_high_bound r)
         else raw_elements_between above_low_bound below_high_bound l
    else raw_elements_between above_low_bound below_high_bound r

  (** val elements_between : (elt -> bool) -> (elt -> bool) -> t -> t **)

  let elements_between above_low_bound below_high_bound s =
    raw_elements_between above_low_bound below_high_bound (MSet.this s)

  (** val raw_for_all_between :
      (elt -> bool) -> (elt -> bool) -> (elt -> bool) -> Raw.tree -> bool **)

  let rec raw_for_all_between pred above_low_bound below_high_bound = function
  | Raw.Leaf -> true
  | Raw.Node (_, l, x, r) ->
    if above_low_bound x
    then if below_high_bound x
         then (&&)
                ((&&)
                  (raw_for_all_between pred above_low_bound below_high_bound
                    l) (pred x))
                (raw_for_all_between pred above_low_bound below_high_bound r)
         else raw_for_all_between pred above_low_bound below_high_bound l
    else raw_for_all_between pred above_low_bound below_high_bound r

  (** val for_all_between :
      (elt -> bool) -> (elt -> bool) -> (elt -> bool) -> t -> bool **)

  let for_all_between pred above_low_bound below_high_bound m =
    raw_for_all_between pred above_low_bound below_high_bound (MSet.this m)

  (** val raw_partition_between :
      (elt -> bool) -> (elt -> bool) -> Raw.tree -> Raw.tree * Raw.tree **)

  let rec raw_partition_between above_low_bound below_high_bound = function
  | Raw.Leaf -> (Raw.Leaf, Raw.Leaf)
  | Raw.Node (_, l, x, r) ->
    if above_low_bound x
    then if below_high_bound x
         then let (l1, l2) =
                raw_partition_between above_low_bound below_high_bound l
              in
              let (r1, r2) =
                raw_partition_between above_low_bound below_high_bound r
              in
              ((Raw.join l1 x r1), (Raw.concat l2 r2))
         else let (l1, l2) =
                raw_partition_between above_low_bound below_high_bound l
              in
              (l1, (Raw.join l2 x r))
    else let (r1, r2) =
           raw_partition_between above_low_bound below_high_bound r
         in
         (r1, (Raw.join l x r2))

  (** val partition_between : (elt -> bool) -> (elt -> bool) -> t -> t * t **)

  let partition_between above_low_bound below_high_bound s =
    let p =
      raw_partition_between above_low_bound below_high_bound (MSet.this s)
    in
    ((fst p), (snd p))
 end
