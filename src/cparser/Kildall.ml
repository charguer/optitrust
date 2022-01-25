open BinNums
open Coqlib
open Datatypes
open Heaps
open Iteration
open Lattice
open List0
open Maps

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module type NODE_SET =
 sig
  type t

  val empty : t

  val add : positive -> t -> t

  val pick : t -> (positive * t) option

  val all_nodes : 'a1 PTree.t -> t
 end

module Dataflow_Solver =
 functor (LAT:SEMILATTICE) ->
 functor (NS:NODE_SET) ->
 struct
  module L = LAT

  type state = { aval : L.t PTree.t; worklist : NS.t }

  (** val aval : state -> L.t PTree.t **)

  let aval s =
    s.aval

  (** val worklist : state -> NS.t **)

  let worklist s =
    s.worklist

  (** val abstr_value : positive -> state -> L.t **)

  let abstr_value n s =
    match PTree.get n (aval s) with
    | Some v -> v
    | None -> L.bot

  (** val propagate_succ : state -> L.t -> positive -> state **)

  let propagate_succ s out n =
    match PTree.get n (aval s) with
    | Some oldl ->
      let newl = L.lub oldl out in
      if L.beq oldl newl
      then s
      else { aval = (PTree.set n newl (aval s)); worklist =
             (NS.add n (worklist s)) }
    | None ->
      { aval = (PTree.set n out (aval s)); worklist =
        (NS.add n (worklist s)) }

  (** val propagate_succ_list : state -> L.t -> positive list -> state **)

  let rec propagate_succ_list s out = function
  | [] -> s
  | n :: rem -> propagate_succ_list (propagate_succ s out n) out rem

  (** val step :
      'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
      state -> (L.t PMap.t, state) sum **)

  let step code successors transf s =
    match NS.pick (worklist s) with
    | Some p ->
      let (n, rem) = p in
      (match PTree.get n code with
       | Some instr ->
         Coq_inr
           (propagate_succ_list { aval = (aval s); worklist = rem }
             (transf n (abstr_value n s)) (successors instr))
       | None -> Coq_inr { aval = (aval s); worklist = rem })
    | None -> Coq_inl (L.bot, (aval s))

  (** val fixpoint_from :
      'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
      state -> L.t PMap.t option **)

  let fixpoint_from code successors transf start =
    PrimIter.iterate (step code successors transf) start

  (** val start_state : positive -> L.t -> state **)

  let start_state enode eval =
    { aval = (PTree.set enode eval PTree.empty); worklist =
      (NS.add enode NS.empty) }

  (** val fixpoint :
      'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
      positive -> L.t -> L.t PMap.t option **)

  let fixpoint code successors transf enode eval =
    fixpoint_from code successors transf (start_state enode eval)

  (** val start_state_nodeset : NS.t -> state **)

  let start_state_nodeset enodes =
    { aval = PTree.empty; worklist = enodes }

  (** val fixpoint_nodeset :
      'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
      NS.t -> L.t PMap.t option **)

  let fixpoint_nodeset code successors transf enodes =
    fixpoint_from code successors transf (start_state_nodeset enodes)

  (** val start_state_allnodes : 'a1 PTree.t -> state **)

  let start_state_allnodes code =
    { aval = PTree.empty; worklist = (NS.all_nodes code) }

  (** val fixpoint_allnodes :
      'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
      L.t PMap.t option **)

  let fixpoint_allnodes code successors transf =
    fixpoint_from code successors transf (start_state_allnodes code)
 end

(** val successors_list :
    positive list PTree.t -> positive -> positive list **)

let successors_list successors pc =
  match PTree.get pc successors with
  | Some l -> l
  | None -> []

(** val add_successors :
    positive list PTree.t -> positive -> positive list -> positive list
    PTree.t **)

let rec add_successors pred from = function
| [] -> pred
| to0 :: rem ->
  add_successors (PTree.set to0 (from :: (successors_list pred to0)) pred)
    from rem

(** val make_predecessors :
    'a1 PTree.t -> ('a1 -> positive list) -> positive list PTree.t **)

let make_predecessors code successors =
  PTree.fold (fun pred pc instr -> add_successors pred pc (successors instr))
    code PTree.empty

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

module Backward_Dataflow_Solver =
 functor (LAT:SEMILATTICE) ->
 functor (NS:NODE_SET) ->
 struct
  module L = LAT

  module DS = Dataflow_Solver(L)(NS)

  (** val sequential_node :
      'a1 PTree.t -> ('a1 -> positive list) -> positive -> 'a1 -> bool **)

  let sequential_node code successors pc instr =
    existsb (fun s ->
      match PTree.get s code with
      | Some _ -> (fun x -> x) (plt s pc)
      | None -> false) (successors instr)

  (** val exit_points : 'a1 PTree.t -> ('a1 -> positive list) -> NS.t **)

  let exit_points code successors =
    PTree.fold (fun ep pc instr ->
      if sequential_node code successors pc instr then ep else NS.add pc ep)
      code NS.empty

  (** val fixpoint :
      'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
      DS.L.t PMap.t option **)

  let fixpoint code successors transf =
    DS.fixpoint_nodeset (make_predecessors code successors) (fun l -> l)
      transf (exit_points code successors)

  (** val fixpoint_solution : __ **)

  let fixpoint_solution =
    __

  (** val fixpoint_allnodes :
      'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
      DS.L.t PMap.t option **)

  let fixpoint_allnodes code successors transf =
    DS.fixpoint_allnodes (make_predecessors code successors) (fun l -> l)
      transf

  (** val fixpoint_allnodes_solution : __ **)

  let fixpoint_allnodes_solution =
    __
 end

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

module BBlock_solver =
 functor (LAT:ORDERED_TYPE_WITH_TOP) ->
 struct
  module L = LAT

  type bbmap = positive -> bool

  type result = L.t PMap.t

  type state = { aval : result; worklist : positive list }

  (** val aval : state -> result **)

  let aval s =
    s.aval

  (** val worklist : state -> positive list **)

  let worklist s =
    s.worklist

  (** val propagate_successors :
      bbmap -> positive list -> L.t -> state -> state **)

  let rec propagate_successors bb succs l st =
    match succs with
    | [] -> st
    | s1 :: sl ->
      if bb s1
      then propagate_successors bb sl l st
      else propagate_successors bb sl l { aval = (PMap.set s1 l (aval st));
             worklist = (s1 :: (worklist st)) }

  (** val step :
      'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
      bbmap -> state -> (result, state) sum **)

  let step code successors transf bb st =
    match worklist st with
    | [] -> Coq_inl (aval st)
    | pc :: rem ->
      (match PTree.get pc code with
       | Some instr ->
         Coq_inr
           (propagate_successors bb (successors instr)
             (transf pc (PMap.get pc (aval st))) { aval = (aval st);
             worklist = rem })
       | None -> Coq_inr { aval = (aval st); worklist = rem })

  (** val is_basic_block_head :
      positive -> positive list PTree.t -> positive -> bool **)

  let is_basic_block_head entrypoint preds pc =
    if peq pc entrypoint
    then true
    else (match successors_list preds pc with
          | [] -> false
          | s :: l ->
            (match l with
             | [] -> (fun x -> x) (peq s pc)
             | _ :: _ -> true))

  (** val basic_block_map :
      'a1 PTree.t -> ('a1 -> positive list) -> positive -> bbmap **)

  let basic_block_map code successors entrypoint =
    is_basic_block_head entrypoint (make_predecessors code successors)

  (** val basic_block_list : 'a1 PTree.t -> bbmap -> positive list **)

  let basic_block_list code bb =
    PTree.fold (fun l pc _ -> if bb pc then pc :: l else l) code []

  (** val fixpoint :
      'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
      positive -> result option **)

  let fixpoint code successors transf entrypoint =
    let bb = basic_block_map code successors entrypoint in
    PrimIter.iterate (step code successors transf bb) { aval =
      (PMap.init L.top); worklist = (basic_block_list code bb) }

  (** val fixpoint_solution : __ **)

  let fixpoint_solution =
    __

  (** val fixpoint_entry : __ **)

  let fixpoint_entry =
    __

  (** val fixpoint_invariant : __ **)

  let fixpoint_invariant =
    __
 end

module NodeSetForward =
 struct
  type t = PHeap.t

  (** val empty : PHeap.t **)

  let empty =
    PHeap.empty

  (** val add : positive -> t -> t **)

  let add =
    PHeap.insert

  (** val pick : t -> (positive * PHeap.t) option **)

  let pick s =
    match PHeap.findMax s with
    | Some n -> Some (n, (PHeap.deleteMax s))
    | None -> None

  (** val all_nodes : 'a1 PTree.t -> PHeap.t **)

  let all_nodes code =
    PTree.fold (fun s pc _ -> PHeap.insert pc s) code PHeap.empty
 end

module NodeSetBackward =
 struct
  type t = PHeap.t

  (** val empty : PHeap.t **)

  let empty =
    PHeap.empty

  (** val add : positive -> t -> t **)

  let add =
    PHeap.insert

  (** val pick : t -> (positive * PHeap.t) option **)

  let pick s =
    match PHeap.findMin s with
    | Some n -> Some (n, (PHeap.deleteMin s))
    | None -> None

  (** val all_nodes : 'a1 PTree.t -> PHeap.t **)

  let all_nodes code =
    PTree.fold (fun s pc _ -> PHeap.insert pc s) code PHeap.empty
 end
