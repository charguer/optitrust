open AST
open BinNums
open Builtins
open Builtins0
open Cminor
open CminorSel
open Cminortyping
open Coqlib
open Datatypes
open Errors
open Integers
open List0
open Machregs
open Maps
open Op
open SelectDiv
open SelectLong
open SelectOp
open SplitLong
open String0
open Switch

val condexpr_of_expr : expr -> condexpr

val condition_of_expr : expr -> condition * exprlist

val load : memory_chunk -> expr -> expr

val store : memory_chunk -> expr -> expr -> stmt

type globdef = (Cminor.fundef, unit) AST.globdef

val sel_constant : constant -> expr

val sel_unop : helper_functions -> unary_operation -> expr -> expr

val sel_binop : helper_functions -> binary_operation -> expr -> expr -> expr

val sel_select : typ -> expr -> expr -> expr -> expr

val sel_expr : helper_functions -> Cminor.expr -> expr

val sel_exprlist : helper_functions -> Cminor.expr list -> exprlist

val sel_select_opt :
  helper_functions -> typ -> Cminor.expr -> Cminor.expr -> Cminor.expr ->
  expr option

type call_kind =
| Call_default
| Call_imm of ident
| Call_builtin of external_function

val expr_is_addrof_ident : Cminor.expr -> ident option

val classify_call : globdef PTree.t -> Cminor.expr -> call_kind

val sel_builtin_arg :
  helper_functions -> Cminor.expr -> builtin_arg_constraint -> expr
  builtin_arg

val sel_builtin_args :
  helper_functions -> Cminor.expr list -> builtin_arg_constraint list -> expr
  builtin_arg list

val sel_builtin_res : ident option -> ident builtin_res

val sel_known_builtin : builtin_function -> exprlist -> expr option

val coq_Sno_op : stmt

val sel_builtin_default :
  helper_functions -> ident option -> external_function -> Cminor.expr list
  -> stmt

val sel_builtin :
  helper_functions -> ident option -> external_function -> Cminor.expr list
  -> stmt

val compile_switch : coq_Z -> nat -> table -> comptree

val sel_switch :
  (expr -> coq_Z -> expr) -> (expr -> coq_Z -> expr) -> (expr -> coq_Z ->
  expr) -> (expr -> expr) -> nat -> comptree -> exitexpr

val sel_switch_int : nat -> comptree -> exitexpr

val sel_switch_long : nat -> comptree -> exitexpr

type stmt_class =
| SCskip
| SCassign of ident * Cminor.expr
| SCother

val classify_stmt : Cminor.stmt -> stmt_class

val if_conversion_heuristic :
  Cminor.expr -> Cminor.expr -> Cminor.expr -> typ -> bool

val if_conversion_base :
  helper_functions -> known_idents -> typenv -> Cminor.expr -> ident ->
  Cminor.expr -> Cminor.expr -> stmt option

val if_conversion :
  helper_functions -> known_idents -> typenv -> Cminor.expr -> Cminor.stmt ->
  Cminor.stmt -> stmt option

val sel_stmt :
  globdef PTree.t -> helper_functions -> known_idents -> typenv ->
  Cminor.stmt -> stmt res

val known_id : Cminor.coq_function -> known_idents

val sel_function :
  globdef PTree.t -> helper_functions -> Cminor.coq_function -> coq_function
  res

val sel_fundef :
  globdef PTree.t -> helper_functions -> Cminor.fundef -> fundef res

val globdef_of_interest : globdef -> bool

val record_globdefs : globdef PTree.t -> globdef PTree.t

val lookup_helper_aux :
  char list -> signature -> ident option -> ident -> globdef -> ident option

val lookup_helper : globdef PTree.t -> char list -> signature -> ident res

val get_helpers : globdef PTree.t -> helper_functions res

val sel_program : Cminor.program -> program res
