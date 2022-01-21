open AST
open BinNums
open Conventions
open Coqlib
open Linear
open List0
open Locations
open Machregs
open Op
open Znumtheory

val slot_valid : coq_function -> slot -> coq_Z -> typ -> bool

val slot_writable : slot -> bool

val loc_valid : coq_function -> loc -> bool

val wt_builtin_res : typ -> mreg builtin_res -> bool

val wt_instr : coq_function -> instruction -> bool

val wt_code : coq_function -> code -> bool

val wt_function : coq_function -> bool
