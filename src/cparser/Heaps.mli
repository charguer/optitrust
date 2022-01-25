open BinNums
open Ordered

module SplayHeapSet :
 functor (E:OrderedType.OrderedType) ->
 sig
  module R :
   sig
    type heap =
    | Empty
    | Node of heap * E.t * heap

    val partition : E.t -> heap -> heap * heap

    val insert : E.t -> heap -> heap

    val findMin : heap -> E.t option

    val deleteMin : heap -> heap

    val findMax : heap -> E.t option

    val deleteMax : heap -> heap

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

  val empty : t

  val insert : E.t -> t -> t

  val findMin : t -> E.t option

  val deleteMin : t -> t

  val findMax : t -> E.t option

  val deleteMax : t -> t
 end

module PHeap :
 sig
  module R :
   sig
    type heap =
    | Empty
    | Node of heap * positive * heap

    val partition : positive -> heap -> heap * heap

    val insert : positive -> heap -> heap

    val findMin : heap -> positive option

    val deleteMin : heap -> heap

    val findMax : heap -> positive option

    val deleteMax : heap -> heap

    type coq_R_partition =
    | R_partition_0 of heap
    | R_partition_1 of heap * heap * positive * heap
    | R_partition_2 of heap * heap * positive * heap * heap * positive * 
       heap * (heap * heap) * coq_R_partition * heap * heap
    | R_partition_3 of heap * heap * positive * heap * heap * positive * heap
    | R_partition_4 of heap * heap * positive * heap * heap * positive * 
       heap * (heap * heap) * coq_R_partition * heap * heap
    | R_partition_5 of heap * heap * positive * heap
    | R_partition_6 of heap * heap * positive * heap
    | R_partition_7 of heap * heap * positive * heap * heap * positive * 
       heap * (heap * heap) * coq_R_partition * heap * heap
    | R_partition_8 of heap * heap * positive * heap * heap * positive * heap
    | R_partition_9 of heap * heap * positive * heap * heap * positive * 
       heap * (heap * heap) * coq_R_partition * heap * heap

    type coq_R_deleteMin =
    | R_deleteMin_0 of heap
    | R_deleteMin_1 of heap * heap * positive * heap
    | R_deleteMin_2 of heap * heap * positive * heap * heap * positive * heap
    | R_deleteMin_3 of heap * heap * positive * heap * heap * positive * 
       heap * heap * positive * heap * heap * coq_R_deleteMin

    type coq_R_deleteMax =
    | R_deleteMax_0 of heap
    | R_deleteMax_1 of heap * heap * positive * heap
    | R_deleteMax_2 of heap * heap * positive * heap * heap * positive * heap
    | R_deleteMax_3 of heap * heap * positive * heap * heap * positive * 
       heap * heap * positive * heap * heap * coq_R_deleteMax
   end

  type t = R.heap

  val empty : t

  val insert : positive -> t -> t

  val findMin : t -> positive option

  val deleteMin : t -> t

  val findMax : t -> positive option

  val deleteMax : t -> t
 end
