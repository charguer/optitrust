open AST
open BinNums
open List0
open Maps
open Postorder
open RTL

val renum_pc : positive PTree.t -> node -> node

val renum_instr : positive PTree.t -> instruction -> instruction

val renum_node : positive PTree.t -> code -> node -> instruction -> code

val renum_cfg : positive PTree.t -> code -> code

val transf_function : coq_function -> coq_function

val transf_fundef : fundef -> fundef

val transf_program : program -> program
