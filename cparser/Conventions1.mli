open AST
open Archi
open BinInt
open BinNums
open Coqlib
open Datatypes
open List0
open Locations
open Machregs

val is_callee_save : mreg -> bool

val int_caller_save_regs : mreg list

val float_caller_save_regs : mreg list

val int_callee_save_regs : mreg list

val float_callee_save_regs : mreg list

val destroyed_at_call : mreg list

val is_float_reg : mreg -> bool

val dummy_int_reg : mreg

val dummy_float_reg : mreg

val callee_save_type : mreg -> typ

val loc_result_32 : signature -> mreg rpair

val loc_result_64 : signature -> mreg rpair

val loc_result : signature -> mreg rpair

val loc_arguments_32 : typ list -> coq_Z -> loc rpair list

val int_param_regs_elf64 : mreg list

val float_param_regs_elf64 : mreg list

val loc_arguments_elf64 :
  typ list -> coq_Z -> coq_Z -> coq_Z -> loc rpair list

val int_param_regs_win64 : mreg list

val float_param_regs_win64 : mreg list

val loc_arguments_win64 : typ list -> coq_Z -> coq_Z -> loc rpair list

val loc_arguments : signature -> loc rpair list

val return_value_needs_normalization : rettype -> bool

val parameter_needs_normalization : rettype -> bool
