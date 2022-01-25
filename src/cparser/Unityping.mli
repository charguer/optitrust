open BinNums
open Coqlib
open Datatypes
open Errors
open Maps

type __ = Obj.t

module type TYPE_ALGEBRA =
 sig
  type t

  val eq : t -> t -> bool

  val default : t
 end

module UniSolver :
 functor (T:TYPE_ALGEBRA) ->
 sig
  type coq_constraint = positive * positive

  type typenv = { te_typ : T.t PTree.t; te_equ : coq_constraint list }

  val te_typ : typenv -> T.t PTree.t

  val te_equ : typenv -> coq_constraint list

  val initial : typenv

  val set : typenv -> positive -> T.t -> typenv res

  val set_list : typenv -> positive list -> T.t list -> typenv res

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

  type typassign = positive -> T.t

  val makeassign : typenv -> typassign

  val solve : typenv -> typassign res
 end
