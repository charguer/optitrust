open AST
open BinInt
open BinNums
open Conventions
open Coqlib
open Datatypes
open Errors
open Integers
open Maps
open Op
open RTL
open Registers
open Unityping

type __ = Obj.t

type regenv = reg -> typ

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

val check_successor : coq_function -> node -> unit res

val check_successors : coq_function -> node list -> unit res

val type_ros : S.typenv -> (reg, ident) sum -> S.typenv res

val is_move : operation -> bool

val type_expect : S.typenv -> typ -> typ -> S.typenv res

val type_builtin_arg : S.typenv -> reg builtin_arg -> typ -> S.typenv res

val type_builtin_args :
  S.typenv -> reg builtin_arg list -> typ list -> S.typenv res

val type_builtin_res : S.typenv -> reg builtin_res -> typ -> S.typenv res

val type_instr : coq_function -> S.typenv -> instruction -> S.typenv res

val type_code : coq_function -> S.typenv -> S.typenv res

val check_params_norepet : reg list -> unit res

val type_function : coq_function -> regenv res
