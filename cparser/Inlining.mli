open AST
open BinInt
open BinNums
open BinPos
open Coqlib
open Datatypes
open Errors
open Integers
open List0
open Maps
open Op
open RTL
open Registers

type __ = Obj.t

type funenv = coq_function PTree.t

val add_globdef :
  Inliningaux.inlining_info -> funenv -> (ident * (fundef, unit) globdef) ->
  funenv

val funenv_program : program -> funenv

type state = { st_nextreg : positive; st_nextnode : positive; st_code : 
               code; st_stksize : coq_Z }

type 'a res =
| R of 'a * state

type 'a mon = state -> 'a res

val initstate : state

val set_instr : node -> instruction -> unit mon

val add_instr : instruction -> node mon

val reserve_nodes : positive -> positive mon

val reserve_regs : positive -> positive mon

val request_stack : coq_Z -> unit mon

val ptree_mfold : (positive -> 'a1 -> unit mon) -> 'a1 PTree.t -> unit mon

type context = { dpc : positive; dreg : positive; dstk : coq_Z;
                 mreg : positive; mstk : coq_Z; retinfo : (node * reg) option }

val shiftpos : positive -> positive -> positive

val spc : context -> node -> positive

val sreg : context -> reg -> positive

val sregs : context -> reg list -> positive list

val sros : context -> (reg, ident) sum -> (positive, ident) sum

val sop : context -> operation -> operation

val saddr : context -> addressing -> addressing

val sbuiltinarg : context -> reg builtin_arg -> reg builtin_arg

val sbuiltinres : context -> reg builtin_res -> reg builtin_res

val initcontext : positive -> positive -> positive -> coq_Z -> context

val min_alignment : coq_Z -> coq_Z

val callcontext :
  context -> positive -> positive -> positive -> coq_Z -> node -> reg ->
  context

val tailcontext :
  context -> positive -> positive -> positive -> coq_Z -> context

val add_moves : reg list -> reg list -> node -> node mon

type inline_decision =
| Cannot_inline
| Can_inline of ident * coq_function

val can_inline : funenv -> (reg, ident) sum -> inline_decision

val inline_function :
  funenv -> (funenv -> __ -> context -> coq_function -> unit mon) -> context
  -> ident -> coq_function -> reg list -> node -> reg -> node mon

val inline_tail_function :
  funenv -> (funenv -> __ -> context -> coq_function -> unit mon) -> context
  -> ident -> coq_function -> reg list -> node mon

val inline_return : context -> reg option -> (node * reg) -> instruction

val expand_instr :
  funenv -> (funenv -> __ -> context -> coq_function -> unit mon) -> context
  -> node -> instruction -> unit mon

val expand_cfg_rec :
  funenv -> (funenv -> __ -> context -> coq_function -> unit mon) -> context
  -> coq_function -> unit mon

val expand_cfg : funenv -> context -> coq_function -> unit mon

val expand_function : funenv -> coq_function -> context mon

val transf_function : funenv -> coq_function -> coq_function Errors.res

val transf_fundef : funenv -> fundef -> fundef Errors.res

val transf_program : program -> program Errors.res
