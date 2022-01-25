open AST
open BinNums
open Cminor
open Datatypes
open Errors
open Maps
open Unityping

type __ = Obj.t

val type_constant : constant -> typ

val type_unop : unary_operation -> typ * typ

val type_binop : binary_operation -> (typ * typ) * typ

module RTLtypes :
 sig
  type t = typ

  val eq : typ -> typ -> bool

  val default : typ
 end

module S :
 sig
  type coq_constraint = positive * positive

  type typenv = { te_typ : RTLtypes.t PTree.t; te_equ : coq_constraint list }

  val te_typ : typenv -> RTLtypes.t PTree.t

  val te_equ : typenv -> coq_constraint list

  val initial : typenv

  val set : typenv -> positive -> RTLtypes.t -> typenv res

  val set_list : typenv -> positive list -> RTLtypes.t list -> typenv res

  val move : typenv -> positive -> positive -> (bool * typenv) res

  val solve_rec : typenv -> bool -> coq_constraint list -> (typenv * bool) res

  val weight_typenv : typenv -> nat

  val solve_constraints_F : (typenv -> typenv res) -> typenv -> typenv res

  val solve_constraints_terminate : typenv -> typenv res

  val solve_constraints : typenv -> typenv res

  type coq_R_solve_constraints =
  | R_solve_constraints_0 of typenv * typenv
  | R_solve_constraints_1 of typenv * typenv * typenv res
     * coq_R_solve_constraints
  | R_solve_constraints_2 of typenv * errmsg

  val coq_R_solve_constraints_rect :
    (typenv -> typenv -> __ -> 'a1) -> (typenv -> typenv -> __ -> typenv res
    -> coq_R_solve_constraints -> 'a1 -> 'a1) -> (typenv -> errmsg -> __ ->
    'a1) -> typenv -> typenv res -> coq_R_solve_constraints -> 'a1

  val coq_R_solve_constraints_rec :
    (typenv -> typenv -> __ -> 'a1) -> (typenv -> typenv -> __ -> typenv res
    -> coq_R_solve_constraints -> 'a1 -> 'a1) -> (typenv -> errmsg -> __ ->
    'a1) -> typenv -> typenv res -> coq_R_solve_constraints -> 'a1

  val solve_constraints_rect :
    (typenv -> typenv -> __ -> 'a1) -> (typenv -> typenv -> __ -> 'a1 -> 'a1)
    -> (typenv -> errmsg -> __ -> 'a1) -> typenv -> 'a1

  val solve_constraints_rec :
    (typenv -> typenv -> __ -> 'a1) -> (typenv -> typenv -> __ -> 'a1 -> 'a1)
    -> (typenv -> errmsg -> __ -> 'a1) -> typenv -> 'a1

  val coq_R_solve_constraints_correct :
    typenv -> typenv res -> coq_R_solve_constraints

  type typassign = positive -> RTLtypes.t

  val makeassign : typenv -> typassign

  val solve : typenv -> typassign res
 end

val expect : S.typenv -> typ -> typ -> S.typenv res

val type_expr : S.typenv -> expr -> typ -> S.typenv res

val type_exprlist : S.typenv -> expr list -> typ list -> S.typenv res

val type_assign : S.typenv -> ident -> expr -> S.typenv res

val opt_set : S.typenv -> ident option -> typ -> S.typenv res

val type_stmt : rettype -> S.typenv -> stmt -> S.typenv res

type typenv = ident -> typ

val type_function : coq_function -> typenv res

type known_idents = unit PTree.t

val is_known : known_idents -> ident -> bool

val safe_unop : unary_operation -> bool

val safe_binop : binary_operation -> bool

val safe_expr : known_idents -> expr -> bool
