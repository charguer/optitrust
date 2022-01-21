open BinNums
open Ordered

module SplayHeapSet =
 functor (E:OrderedType.OrderedType) ->
 struct
  module R =
   struct
    type heap =
    | Empty
    | Node of heap * E.t * heap

    (** val partition : E.t -> heap -> heap * heap **)

    let rec partition pivot h = match h with
    | Empty -> (Empty, Empty)
    | Node (a, x, b) ->
      (match E.compare x pivot with
       | OrderedType.LT ->
         (match b with
          | Empty -> (h, Empty)
          | Node (b1, y, b2) ->
            (match E.compare y pivot with
             | OrderedType.LT ->
               let (small, big) = partition pivot b2 in
               ((Node ((Node (a, x, b1)), y, small)), big)
             | OrderedType.EQ -> ((Node (a, x, b1)), b2)
             | OrderedType.GT ->
               let (small, big) = partition pivot b1 in
               ((Node (a, x, small)), (Node (big, y, b2)))))
       | OrderedType.EQ -> (a, b)
       | OrderedType.GT ->
         (match a with
          | Empty -> (Empty, h)
          | Node (a1, y, a2) ->
            (match E.compare y pivot with
             | OrderedType.LT ->
               let (small, big) = partition pivot a2 in
               ((Node (a1, y, small)), (Node (big, x, b)))
             | OrderedType.EQ -> (a1, (Node (a2, x, b)))
             | OrderedType.GT ->
               let (small, big) = partition pivot a1 in
               (small, (Node (big, y, (Node (a2, x, b))))))))

    (** val insert : E.t -> heap -> heap **)

    let insert x h =
      let (a, b) = partition x h in Node (a, x, b)

    (** val findMin : heap -> E.t option **)

    let rec findMin = function
    | Empty -> None
    | Node (a, x, _) ->
      (match a with
       | Empty -> Some x
       | Node (_, _, _) -> findMin a)

    (** val deleteMin : heap -> heap **)

    let rec deleteMin = function
    | Empty -> Empty
    | Node (l, y, c) ->
      (match l with
       | Empty -> c
       | Node (a, x, b) ->
         (match a with
          | Empty -> Node (b, y, c)
          | Node (_, _, _) -> Node ((deleteMin a), x, (Node (b, y, c)))))

    (** val findMax : heap -> E.t option **)

    let rec findMax = function
    | Empty -> None
    | Node (_, x, b) ->
      (match b with
       | Empty -> Some x
       | Node (_, _, _) -> findMax b)

    (** val deleteMax : heap -> heap **)

    let rec deleteMax = function
    | Empty -> Empty
    | Node (a, x, r) ->
      (match r with
       | Empty -> a
       | Node (b, y, c) ->
         (match c with
          | Empty -> Node (a, x, b)
          | Node (_, _, _) -> Node ((Node (a, x, b)), y, (deleteMax c))))

    type coq_R_partition =
    | R_partition_0 of heap
    | R_partition_1 of heap * heap * E.t * heap
    | R_partition_2 of heap * heap * E.t * heap * heap * E.t * heap
       * (heap * heap) * coq_R_partition * heap * heap
    | R_partition_3 of heap * heap * E.t * heap * heap * E.t * heap
    | R_partition_4 of heap * heap * E.t * heap * heap * E.t * heap
       * (heap * heap) * coq_R_partition * heap * heap
    | R_partition_5 of heap * heap * E.t * heap
    | R_partition_6 of heap * heap * E.t * heap
    | R_partition_7 of heap * heap * E.t * heap * heap * E.t * heap
       * (heap * heap) * coq_R_partition * heap * heap
    | R_partition_8 of heap * heap * E.t * heap * heap * E.t * heap
    | R_partition_9 of heap * heap * E.t * heap * heap * E.t * heap
       * (heap * heap) * coq_R_partition * heap * heap

    type coq_R_deleteMin =
    | R_deleteMin_0 of heap
    | R_deleteMin_1 of heap * heap * E.t * heap
    | R_deleteMin_2 of heap * heap * E.t * heap * heap * E.t * heap
    | R_deleteMin_3 of heap * heap * E.t * heap * heap * E.t * heap * 
       heap * E.t * heap * heap * coq_R_deleteMin

    type coq_R_deleteMax =
    | R_deleteMax_0 of heap
    | R_deleteMax_1 of heap * heap * E.t * heap
    | R_deleteMax_2 of heap * heap * E.t * heap * heap * E.t * heap
    | R_deleteMax_3 of heap * heap * E.t * heap * heap * E.t * heap * 
       heap * E.t * heap * heap * coq_R_deleteMax
   end

  type t = R.heap

  (** val empty : t **)

  let empty =
    R.Empty

  (** val insert : E.t -> t -> t **)

  let insert =
    R.insert

  (** val findMin : t -> E.t option **)

  let findMin =
    R.findMin

  (** val deleteMin : t -> t **)

  let deleteMin =
    R.deleteMin

  (** val findMax : t -> E.t option **)

  let findMax =
    R.findMax

  (** val deleteMax : t -> t **)

  let deleteMax =
    R.deleteMax
 end

module PHeap = SplayHeapSet(OrderedPositive)
