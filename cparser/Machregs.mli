open AST
open Archi
open BinNums
open Coqlib
open Datatypes
open List0
open Op
open String0

type mreg =
| AX
| BX
| CX
| DX
| SI
| DI
| BP
| R8
| R9
| R10
| R11
| R12
| R13
| R14
| R15
| X0
| X1
| X2
| X3
| X4
| X5
| X6
| X7
| X8
| X9
| X10
| X11
| X12
| X13
| X14
| X15
| FP0

val mreg_eq : mreg -> mreg -> bool

val all_mregs : mreg list

val mreg_type : mreg -> typ

module IndexedMreg :
 sig
  type t = mreg

  val eq : mreg -> mreg -> bool

  val index : mreg -> positive
 end

val is_stack_reg : mreg -> bool

val register_names : (char list * mreg) list

val register_by_name : char list -> mreg option

val destroyed_by_op : operation -> mreg list

val destroyed_by_load : memory_chunk -> addressing -> mreg list

val destroyed_by_store : memory_chunk -> addressing -> mreg list

val destroyed_by_cond : condition -> mreg list

val destroyed_by_jumptable : mreg list

val destroyed_by_clobber : char list list -> mreg list

val destroyed_by_builtin : external_function -> mreg list

val destroyed_at_function_entry : mreg list

val destroyed_by_setstack : typ -> mreg list

val destroyed_at_indirect_call : mreg list

val temp_for_parent_frame : mreg

val mregs_for_operation : operation -> mreg option list * mreg option

val mregs_for_builtin :
  external_function -> mreg option list * mreg option list

val two_address_op : operation -> bool

val builtin_constraints : external_function -> builtin_arg_constraint list
