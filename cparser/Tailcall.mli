open AST
open BinNums
open Conventions
open Coqlib
open Datatypes
open Maps
open Op
open RTL
open Registers

val is_return : nat -> coq_function -> node -> reg -> bool

val niter : nat

val transf_instr : coq_function -> node -> instruction -> instruction

val transf_function : coq_function -> coq_function

val transf_fundef : fundef -> fundef

val transf_program : program -> program
