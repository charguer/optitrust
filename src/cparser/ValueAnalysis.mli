open AST
open Archi
open BinInt
open BinNums
open Builtins
open Builtins0
open Compopts
open Datatypes
open Kildall
open List0
open Liveness
open Maps
open Op
open RTL
open Registers
open ValueAOp
open ValueDomain

val areg : aenv -> reg -> aval

val aregs : aenv -> reg list -> aval list

val mafter_public_call : amem

val mafter_private_call : amem -> amem

val analyze_call : amem -> aval list -> aval * amem

val transfer_call : aenv -> amem -> reg list -> reg -> VA.t'

val abuiltin_arg : aenv -> amem -> romem -> reg builtin_arg -> aval

val set_builtin_res : reg builtin_res -> aval -> aenv -> aenv

val transfer_builtin_default :
  aenv -> amem -> romem -> reg builtin_arg list -> reg builtin_res -> VA.t'

val eval_static_builtin_function :
  aenv -> amem -> romem -> builtin_function -> reg builtin_arg list -> aval
  option

val transfer_builtin :
  aenv -> amem -> romem -> external_function -> reg builtin_arg list -> reg
  builtin_res -> VA.t'

val transfer : coq_function -> romem -> node -> aenv -> amem -> VA.t

val transfer' :
  coq_function -> reg list PTree.t -> romem -> node -> VA.t -> VA.t

module DS :
 sig
  module L :
   sig
    type t = VA.t

    val beq : t -> t -> bool

    val bot : t

    val lub : t -> t -> t
   end

  type state = { aval : L.t PTree.t; worklist : NodeSetForward.t }

  val aval : state -> L.t PTree.t

  val worklist : state -> NodeSetForward.t

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

  val start_state_nodeset : NodeSetForward.t -> state

  val fixpoint_nodeset :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
    NodeSetForward.t -> L.t PMap.t option

  val start_state_allnodes : 'a1 PTree.t -> state

  val fixpoint_allnodes :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) -> L.t
    PMap.t option
 end

val mfunction_entry : amem

val analyze : romem -> coq_function -> VA.t PMap.t

val store_init_data : ablock -> coq_Z -> init_data -> ablock

val store_init_data_list : ablock -> coq_Z -> init_data list -> ablock

val definitive_initializer : init_data list -> bool

val alloc_global : romem -> (ident * (fundef, unit) globdef) -> romem

val romem_for : program -> romem

val avalue : VA.t -> reg -> aval

val aaddressing : VA.t -> addressing -> reg list -> aptr

val aaddr_arg : VA.t -> reg builtin_arg -> aptr
