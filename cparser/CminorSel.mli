open AST
open BinNums
open Cminor
open Compare_dec
open Datatypes
open Op

type expr =
| Evar of ident
| Eop of operation * exprlist
| Eload of memory_chunk * addressing * exprlist
| Econdition of condexpr * expr * expr
| Elet of expr * expr
| Eletvar of nat
| Ebuiltin of external_function * exprlist
| Eexternal of ident * signature * exprlist
and exprlist =
| Enil
| Econs of expr * exprlist
and condexpr =
| CEcond of condition * exprlist
| CEcondition of condexpr * condexpr * condexpr
| CElet of expr * condexpr

type exitexpr =
| XEexit of nat
| XEjumptable of expr * nat list
| XEcondition of condexpr * exitexpr * exitexpr
| XElet of expr * exitexpr

type stmt =
| Sskip
| Sassign of ident * expr
| Sstore of memory_chunk * addressing * exprlist * expr
| Scall of ident option * signature * (expr, ident) sum * exprlist
| Stailcall of signature * (expr, ident) sum * exprlist
| Sbuiltin of ident builtin_res * external_function * expr builtin_arg list
| Sseq of stmt * stmt
| Sifthenelse of condexpr * stmt * stmt
| Sloop of stmt
| Sblock of stmt
| Sexit of nat
| Sswitch of exitexpr
| Sreturn of expr option
| Slabel of label * stmt
| Sgoto of label

type coq_function = { fn_sig : signature; fn_params : ident list;
                      fn_vars : ident list; fn_stackspace : coq_Z;
                      fn_body : stmt }

type fundef = coq_function AST.fundef

type program = (fundef, unit) AST.program

val lift_expr : nat -> expr -> expr

val lift_exprlist : nat -> exprlist -> exprlist

val lift_condexpr : nat -> condexpr -> condexpr

val lift : expr -> expr
