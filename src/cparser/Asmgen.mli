open AST
open Archi
open Asm
open BinNums
open Coqlib
open Datatypes
open Errors
open Floats
open Integers
open List0
open Mach
open Machregs
open Op

val ireg_of : mreg -> ireg res

val freg_of : mreg -> freg res

val mk_mov : preg -> preg -> Asm.code -> Asm.code res

val mk_shrximm : Int.int -> Asm.code -> Asm.code res

val mk_shrxlimm : Int.int -> Asm.code -> Asm.code res

val low_ireg : ireg -> bool

val mk_intconv :
  (ireg -> ireg -> Asm.instruction) -> ireg -> ireg -> Asm.code ->
  Asm.instruction list res

val addressing_mentions : addrmode -> ireg -> bool

val mk_storebyte : addrmode -> ireg -> Asm.code -> Asm.instruction list res

val loadind :
  ireg -> Ptrofs.int -> typ -> mreg -> Asm.code -> Asm.instruction list res

val storeind :
  mreg -> ireg -> Ptrofs.int -> typ -> Asm.code -> Asm.instruction list res

val transl_addressing : addressing -> mreg list -> addrmode res

val normalize_addrmode_32 : addrmode -> addrmode

val normalize_addrmode_64 : addrmode -> addrmode * Int64.int option

val floatcomp : comparison -> freg -> freg -> Asm.instruction

val floatcomp32 : comparison -> freg -> freg -> Asm.instruction

val transl_cond : condition -> mreg list -> Asm.code -> Asm.code res

val testcond_for_signed_comparison : comparison -> testcond

val testcond_for_unsigned_comparison : comparison -> testcond

type extcond =
| Cond_base of testcond
| Cond_or of testcond * testcond
| Cond_and of testcond * testcond

val testcond_for_condition : condition -> extcond

val mk_setcc_base : extcond -> ireg -> Asm.code -> Asm.instruction list

val mk_setcc : extcond -> ireg -> Asm.code -> Asm.instruction list

val mk_jcc : extcond -> Asm.label -> Asm.code -> Asm.instruction list

val negate_testcond : testcond -> testcond

val mk_sel : extcond -> ireg -> ireg -> Asm.code -> Asm.instruction list res

val transl_sel :
  condition -> mreg list -> ireg -> ireg -> Asm.code -> Asm.code res

val transl_op : operation -> mreg list -> mreg -> Asm.code -> Asm.code res

val transl_load :
  memory_chunk -> addressing -> mreg list -> mreg -> Asm.code -> Asm.code res

val transl_store :
  memory_chunk -> addressing -> mreg list -> mreg -> Asm.code -> Asm.code res

val transl_instr :
  coq_function -> instruction -> bool -> Asm.code -> Asm.instruction list res

val it1_is_parent : bool -> instruction -> bool

val transl_code_rec :
  coq_function -> instruction list -> bool -> (Asm.code -> Asm.code res) ->
  Asm.code res

val transl_code' : coq_function -> instruction list -> bool -> Asm.code res

val transl_function : coq_function -> Asm.coq_function res

val transf_function : coq_function -> Asm.coq_function res

val transf_fundef : fundef -> Asm.fundef res

val transf_program : program -> Asm.program res
