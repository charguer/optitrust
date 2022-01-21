open BinNums
open Coqlib
open Datatypes
open Heaps
open Iteration
open Lattice
open List0
open Maps

type __ = Obj.t

module type NODE_SET =
 sig
  type t

  val empty : t

  val add : positive -> t -> t

  val pick : t -> (positive * t) option

  val all_nodes : 'a1 PTree.t -> t
 end

module Dataflow_Solver :
 functor (LAT:SEMILATTICE) ->
 functor (NS:NODE_SET) ->
 sig
  module L :
   sig
    type t = LAT.t

    val beq : t -> t -> bool

    val bot : t

    val lub : t -> t -> t
   end

  type state = { aval : L.t PTree.t; worklist : NS.t }

  val aval : state -> L.t PTree.t

  val worklist : state -> NS.t

  val abstr_value : positive -> state -> L.t

  val propagate_succ : state -> L.t -> positive -> state

  val propagate_succ_list : state -> L.t -> positive list -> state

  val step :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
    state -> (L.t PMap.t, state) sum

  val fixpoint_from :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
    state -> L.t PMap.t option

  val start_state : positive -> L.t -> state

  val fixpoint :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
    positive -> L.t -> L.t PMap.t option

  val start_state_nodeset : NS.t -> state

  val fixpoint_nodeset :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) -> NS.t
    -> L.t PMap.t option

  val start_state_allnodes : 'a1 PTree.t -> state

  val fixpoint_allnodes :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) -> L.t
    PMap.t option
 end

val successors_list : positive list PTree.t -> positive -> positive list

val add_successors :
  positive list PTree.t -> positive -> positive list -> positive list PTree.t

val make_predecessors :
  'a1 PTree.t -> ('a1 -> positive list) -> positive list PTree.t

module type BACKWARD_DATAFLOW_SOLVER =
 sig
  module L :
   SEMILATTICE

  val fixpoint :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) -> L.t
    PMap.t option

  val fixpoint_allnodes :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) -> L.t
    PMap.t option
 end

module Backward_Dataflow_Solver :
 functor (LAT:SEMILATTICE) ->
 functor (NS:NODE_SET) ->
 BACKWARD_DATAFLOW_SOLVER with module L = LAT

module type ORDERED_TYPE_WITH_TOP =
 sig
  type t

  val top : t
 end

module type BBLOCK_SOLVER =
 sig
  module L :
   ORDERED_TYPE_WITH_TOP

  val fixpoint :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
    positive -> L.t PMap.t option
 end

module BBlock_solver :
 functor (LAT:ORDERED_TYPE_WITH_TOP) ->
 BBLOCK_SOLVER with module L = LAT

module NodeSetForward :
 sig
  type t = PHeap.t

  val empty : PHeap.t

  val add : positive -> t -> t

  val pick : t -> (positive * PHeap.t) option

  val all_nodes : 'a1 PTree.t -> PHeap.t
 end

module NodeSetBackward :
 sig
  type t = PHeap.t

  val empty : PHeap.t

  val add : positive -> t -> t

  val pick : t -> (positive * PHeap.t) option

  val all_nodes : 'a1 PTree.t -> PHeap.t
 end
