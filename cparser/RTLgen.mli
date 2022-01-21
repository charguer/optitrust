open AST
open BinNums
open BinPos
open Cminor
open CminorSel
open Coqlib
open Datatypes
open Errors
open Integers
open List0
open Maps
open Op
open RTL
open Registers

type mapping = { map_vars : reg PTree.t; map_letvars : reg list }

type state = { st_nextreg : positive; st_nextnode : positive; st_code : code }

type 'a res =
| Error of errmsg
| OK of 'a * state

type 'a mon = state -> 'a res

val handle_error : 'a1 mon -> 'a1 mon -> 'a1 mon

val init_state : state

val add_instr : instruction -> node mon

val reserve_instr : node mon

val check_empty_node : state -> node -> bool

val update_instr : node -> instruction -> unit mon

val new_reg : reg mon

val init_mapping : mapping

val add_var : mapping -> ident -> (reg * mapping) mon

val add_vars : mapping -> ident list -> (reg list * mapping) mon

val find_var : mapping -> ident -> reg mon

val add_letvar : mapping -> reg -> mapping

val find_letvar : mapping -> nat -> reg mon

val alloc_reg : mapping -> expr -> reg mon

val alloc_regs : mapping -> exprlist -> reg list mon

val alloc_optreg : mapping -> ident option -> reg mon

val add_move : reg -> reg -> node -> node mon

val exprlist_of_expr_list : expr list -> exprlist

val convert_builtin_arg :
  expr builtin_arg -> 'a1 list -> 'a1 builtin_arg * 'a1 list

val convert_builtin_args :
  expr builtin_arg list -> 'a1 list -> 'a1 builtin_arg list

val convert_builtin_res :
  mapping -> rettype -> ident builtin_res -> reg builtin_res mon

val transl_expr : mapping -> expr -> reg -> node -> node mon

val transl_exprlist : mapping -> exprlist -> reg list -> node -> node mon

val transl_condexpr : mapping -> condexpr -> node -> node -> node mon

val transl_exit : node list -> nat -> node mon

val transl_jumptable : node list -> nat list -> node list mon

val transl_exitexpr : mapping -> exitexpr -> node list -> node mon

val more_likely : condexpr -> stmt -> stmt -> bool

type labelmap = node PTree.t

val transl_stmt :
  mapping -> stmt -> node -> node list -> labelmap -> node -> reg option ->
  node mon

val alloc_label : label -> (labelmap * state) -> labelmap * state

val reserve_labels : stmt -> (labelmap * state) -> labelmap * state

val ret_reg : signature -> reg -> reg option

val transl_fun : CminorSel.coq_function -> labelmap -> (node * reg list) mon

val transl_function : CminorSel.coq_function -> coq_function Errors.res

val transl_fundef :
  CminorSel.coq_function AST.fundef -> coq_function AST.fundef Errors.res

val transl_program : CminorSel.program -> program Errors.res
