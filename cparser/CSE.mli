open AST
open BinInt
open BinNums
open BinPos
open Builtins
open CSEdomain
open CombineOp
open Coqlib
open Datatypes
open Errors
open Integers
open Kildall
open List0
open Maps
open Memdata
open Op
open RTL
open Registers
open ValueAnalysis
open ValueDomain

val valnum_reg : numbering -> reg -> numbering * valnum

val valnum_regs : numbering -> reg list -> numbering * valnum list

val find_valnum_rhs : rhs -> equation list -> valnum option

val find_valnum_rhs' : rhs -> equation list -> valnum option

val find_valnum_num : valnum -> equation list -> rhs option

val reg_valnum : numbering -> valnum -> reg option

val regs_valnums : numbering -> valnum list -> reg list option

val find_rhs : numbering -> rhs -> reg option

val forget_reg : numbering -> reg -> reg list PMap.t

val update_reg : numbering -> reg -> valnum -> reg list PMap.t

val add_rhs : numbering -> reg -> rhs -> numbering

val add_op : numbering -> reg -> operation -> reg list -> numbering

val add_load :
  numbering -> reg -> memory_chunk -> addressing -> reg list -> numbering

val set_unknown : numbering -> reg -> numbering

val set_res_unknown : numbering -> reg builtin_res -> numbering

val kill_eqs : (rhs -> bool) -> equation list -> equation list

val kill_equations : (rhs -> bool) -> numbering -> numbering

val filter_loads : rhs -> bool

val kill_all_loads : numbering -> numbering

val filter_after_store : VA.t -> numbering -> aptr -> coq_Z -> rhs -> bool

val kill_loads_after_store :
  VA.t -> numbering -> memory_chunk -> addressing -> reg list -> numbering

val store_normalized_range : memory_chunk -> aval

val add_store_result :
  VA.t -> numbering -> memory_chunk -> addressing -> reg list -> reg ->
  numbering

val kill_loads_after_storebytes :
  VA.t -> numbering -> aptr -> coq_Z -> numbering

val shift_memcpy_eq : coq_Z -> coq_Z -> coq_Z -> equation -> equation option

val add_memcpy_eqs :
  coq_Z -> coq_Z -> coq_Z -> equation list -> equation list -> equation list

val add_memcpy : numbering -> numbering -> aptr -> aptr -> coq_Z -> numbering

val reduce_rec :
  ((valnum -> rhs option) -> 'a1 -> valnum list -> ('a1 * valnum list)
  option) -> numbering -> nat -> 'a1 -> valnum list -> ('a1 * reg list) option

val reduce :
  ((valnum -> rhs option) -> 'a1 -> valnum list -> ('a1 * valnum list)
  option) -> numbering -> 'a1 -> reg list -> valnum list -> 'a1 * reg list

module Numbering :
 sig
  type t = numbering

  val top : numbering
 end

module Solver :
 sig
  module L :
   ORDERED_TYPE_WITH_TOP

  val fixpoint :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) ->
    positive -> L.t PMap.t option
 end

val transfer : coq_function -> VA.t PMap.t -> node -> numbering -> numbering

val analyze : coq_function -> VA.t PMap.t -> numbering PMap.t option

val transf_instr : numbering -> instruction -> instruction

val transf_code : numbering PMap.t -> code -> code

val vanalyze : romem -> coq_function -> VA.t PMap.t

val transf_function : romem -> coq_function -> coq_function res

val transf_fundef : romem -> fundef -> fundef res

val transf_program : program -> program res
