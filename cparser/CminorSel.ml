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

(** val lift_expr : nat -> expr -> expr **)

let rec lift_expr p = function
| Evar id -> Evar id
| Eop (op, bl) -> Eop (op, (lift_exprlist p bl))
| Eload (chunk, addr, bl) -> Eload (chunk, addr, (lift_exprlist p bl))
| Econdition (a0, b, c) ->
  Econdition ((lift_condexpr p a0), (lift_expr p b), (lift_expr p c))
| Elet (b, c) -> Elet ((lift_expr p b), (lift_expr (S p) c))
| Eletvar n -> if le_gt_dec p n then Eletvar (S n) else Eletvar n
| Ebuiltin (ef, bl) -> Ebuiltin (ef, (lift_exprlist p bl))
| Eexternal (id, sg, bl) -> Eexternal (id, sg, (lift_exprlist p bl))

(** val lift_exprlist : nat -> exprlist -> exprlist **)

and lift_exprlist p = function
| Enil -> Enil
| Econs (b, cl) -> Econs ((lift_expr p b), (lift_exprlist p cl))

(** val lift_condexpr : nat -> condexpr -> condexpr **)

and lift_condexpr p = function
| CEcond (c, al) -> CEcond (c, (lift_exprlist p al))
| CEcondition (a0, b, c) ->
  CEcondition ((lift_condexpr p a0), (lift_condexpr p b), (lift_condexpr p c))
| CElet (a0, b) -> CElet ((lift_expr p a0), (lift_condexpr (S p) b))

(** val lift : expr -> expr **)

let lift a =
  lift_expr O a
