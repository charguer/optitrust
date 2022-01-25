open AST
open Builtins
open Compopts
open ConstpropOp
open Coqlib
open Datatypes
open Integers
open List0
open Machregs
open Maps
open Op
open RTL
open Registers
open ValueAOp
open ValueAnalysis
open ValueDomain

val transf_ros : AE.t -> (reg, ident) sum -> (reg, ident) sum

val successor_rec : nat -> coq_function -> AE.t -> node -> node

val num_iter : nat

val successor : coq_function -> AE.t -> node -> node

val builtin_arg_reduction : AE.t -> reg builtin_arg -> reg builtin_arg

val builtin_arg_strength_reduction :
  AE.t -> reg builtin_arg -> builtin_arg_constraint -> reg builtin_arg

val builtin_args_strength_reduction :
  AE.t -> reg builtin_arg list -> builtin_arg_constraint list -> reg
  builtin_arg list

val debug_strength_reduction :
  AE.t -> reg builtin_arg list -> reg builtin_arg list

val builtin_strength_reduction :
  AE.t -> external_function -> reg builtin_arg list -> reg builtin_arg list

val transf_instr :
  coq_function -> VA.t PMap.t -> romem -> node -> instruction -> instruction

val transf_function : romem -> coq_function -> coq_function

val transf_fundef : romem -> fundef -> fundef

val transf_program : program -> program
