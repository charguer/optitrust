open AST
open BinInt
open BinNums
open Bounds
open Coqlib
open Datatypes
open Errors
open Integers
open Linear
open Lineartyping
open List0
open Locations
open Mach
open Machregs
open Op
open Stacklayout

val offset_local : frame_env -> coq_Z -> coq_Z

val offset_arg : coq_Z -> coq_Z

val save_callee_save_rec : mreg list -> coq_Z -> code -> code

val save_callee_save : frame_env -> code -> code

val restore_callee_save_rec : mreg list -> coq_Z -> code -> code

val restore_callee_save : frame_env -> code -> code

val transl_op : frame_env -> operation -> operation

val transl_addr : frame_env -> addressing -> addressing

val transl_builtin_arg : frame_env -> loc builtin_arg -> mreg builtin_arg

val transl_instr : frame_env -> Linear.instruction -> code -> code

val transl_code : frame_env -> Linear.instruction list -> code

val transl_body : Linear.coq_function -> frame_env -> code

val transf_function : Linear.coq_function -> coq_function res

val transf_fundef : Linear.fundef -> fundef res

val transf_program : Linear.program -> program res
