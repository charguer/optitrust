open AST
open BinInt
open BinNums
open Cminor
open Coqlib
open Csharpminor
open Datatypes
open Errors
open Integers
open List0
open Maps
open Mergesort

type compilenv = coq_Z PTree.t

val var_addr : compilenv -> ident -> Cminor.expr

val transl_constant : constant -> Cminor.constant

val transl_expr : compilenv -> expr -> Cminor.expr res

val transl_exprlist : compilenv -> expr list -> Cminor.expr list res

type exit_env = bool list

val shift_exit : exit_env -> nat -> nat

val switch_table : lbl_stmt -> nat -> (coq_Z * nat) list * nat

val switch_env : lbl_stmt -> exit_env -> exit_env

val transl_stmt : compilenv -> exit_env -> stmt -> Cminor.stmt res

val transl_lblstmt :
  compilenv -> exit_env -> lbl_stmt -> Cminor.stmt -> Cminor.stmt res

val block_alignment : coq_Z -> coq_Z

val assign_variable :
  (compilenv * coq_Z) -> (ident * coq_Z) -> compilenv * coq_Z

val assign_variables :
  (compilenv * coq_Z) -> (ident * coq_Z) list -> compilenv * coq_Z

module VarOrder :
 sig
  type t = ident * coq_Z

  val leb : t -> t -> bool
 end

module VarSort :
 sig
  val merge :
    (ident * coq_Z) list -> (ident * coq_Z) list -> (ident * coq_Z) list

  val merge_list_to_stack :
    (ident * coq_Z) list option list -> (ident * coq_Z) list ->
    (ident * coq_Z) list option list

  val merge_stack : (ident * coq_Z) list option list -> (ident * coq_Z) list

  val iter_merge :
    (ident * coq_Z) list option list -> (ident * coq_Z) list ->
    (ident * coq_Z) list

  val sort : (ident * coq_Z) list -> (ident * coq_Z) list

  val flatten_stack : (ident * coq_Z) list option list -> (ident * coq_Z) list
 end

val build_compilenv : coq_function -> compilenv * coq_Z

val transl_funbody :
  compilenv -> coq_Z -> coq_function -> Cminor.coq_function res

val transl_function : coq_function -> Cminor.coq_function res

val transl_fundef : fundef -> Cminor.fundef res

val transl_program : program -> Cminor.program res
