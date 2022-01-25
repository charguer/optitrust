open AST
open Archi
open BinInt
open BinNums
open Compopts
open Datatypes
open Floats
open Integers
open Op
open Registers
open SelectOp
open ValueAOp
open ValueDomain

val coq_Olea_ptr : addressing -> operation

val const_for_result : aval -> operation option

type cond_strength_reduction_cases =
| Coq_cond_strength_reduction_case1 of comparison * reg * reg * Int.int * aval
| Coq_cond_strength_reduction_case2 of comparison * reg * reg * aval * Int.int
| Coq_cond_strength_reduction_case3 of comparison * reg * reg * Int.int * aval
| Coq_cond_strength_reduction_case4 of comparison * reg * reg * aval * Int.int
| Coq_cond_strength_reduction_case5 of comparison * reg * reg * Int64.int
   * aval
| Coq_cond_strength_reduction_case6 of comparison * reg * reg * aval
   * Int64.int
| Coq_cond_strength_reduction_case7 of comparison * reg * reg * Int64.int
   * aval
| Coq_cond_strength_reduction_case8 of comparison * reg * reg * aval
   * Int64.int
| Coq_cond_strength_reduction_default of condition * reg list * aval list

val cond_strength_reduction_match :
  condition -> reg list -> aval list -> cond_strength_reduction_cases

val cond_strength_reduction :
  condition -> reg list -> aval list -> condition * reg list

val make_cmp_base : condition -> reg list -> aval list -> operation * reg list

val make_cmp_imm_eq :
  condition -> reg list -> aval list -> Int.int -> reg -> aval ->
  operation * reg list

val make_cmp_imm_ne :
  condition -> reg list -> aval list -> Int.int -> reg -> aval ->
  operation * reg list

type make_cmp_cases =
| Coq_make_cmp_case1 of Int.int * reg * aval
| Coq_make_cmp_case2 of Int.int * reg * aval
| Coq_make_cmp_case3 of Int.int * reg * aval
| Coq_make_cmp_case4 of Int.int * reg * aval
| Coq_make_cmp_default of condition * reg list * aval list

val make_cmp_match : condition -> reg list -> aval list -> make_cmp_cases

val make_cmp : condition -> reg list -> aval list -> operation * reg list

val make_select :
  condition -> typ -> reg -> reg -> reg list -> aval list -> operation * reg
  list

type addr_strength_reduction_32_generic_cases =
| Coq_addr_strength_reduction_32_generic_case1 of coq_Z * reg * reg * 
   Int.int * aval
| Coq_addr_strength_reduction_32_generic_case2 of coq_Z * reg * reg * 
   aval * Int.int
| Coq_addr_strength_reduction_32_generic_case3 of coq_Z * coq_Z * reg * 
   reg * aval * Int.int
| Coq_addr_strength_reduction_32_generic_case4 of coq_Z * coq_Z * reg * 
   reg * Int.int * aval
| Coq_addr_strength_reduction_32_generic_default of addressing * reg list
   * aval list

val addr_strength_reduction_32_generic_match :
  addressing -> reg list -> aval list ->
  addr_strength_reduction_32_generic_cases

val addr_strength_reduction_32_generic :
  addressing -> reg list -> aval list -> addressing * reg list

type addr_strength_reduction_32_cases =
| Coq_addr_strength_reduction_32_case1 of coq_Z * reg * ident * Ptrofs.int
| Coq_addr_strength_reduction_32_case2 of coq_Z * reg * Ptrofs.int
| Coq_addr_strength_reduction_32_case3 of coq_Z * reg * reg * ident
   * Ptrofs.int * Int.int
| Coq_addr_strength_reduction_32_case4 of coq_Z * reg * reg * Int.int * 
   ident * Ptrofs.int
| Coq_addr_strength_reduction_32_case5 of coq_Z * reg * reg * Ptrofs.int
   * Int.int
| Coq_addr_strength_reduction_32_case6 of coq_Z * reg * reg * Int.int
   * Ptrofs.int
| Coq_addr_strength_reduction_32_case7 of coq_Z * reg * reg * ident
   * Ptrofs.int * aval
| Coq_addr_strength_reduction_32_case8 of coq_Z * reg * reg * aval * 
   ident * Ptrofs.int
| Coq_addr_strength_reduction_32_case9 of coq_Z * coq_Z * reg * reg * 
   ident * Ptrofs.int * Int.int
| Coq_addr_strength_reduction_32_case10 of coq_Z * coq_Z * reg * reg * 
   ident * Ptrofs.int * aval
| Coq_addr_strength_reduction_32_case11 of ident * Ptrofs.int * reg * Int.int
| Coq_addr_strength_reduction_32_case12 of coq_Z * ident * Ptrofs.int * 
   reg * Int.int
| Coq_addr_strength_reduction_32_default of addressing * reg list * aval list

val addr_strength_reduction_32_match :
  addressing -> reg list -> aval list -> addr_strength_reduction_32_cases

val addr_strength_reduction_32 :
  addressing -> reg list -> aval list -> addressing * reg list

type addr_strength_reduction_64_generic_cases =
| Coq_addr_strength_reduction_64_generic_case1 of coq_Z * reg * reg
   * Int64.int * aval
| Coq_addr_strength_reduction_64_generic_case2 of coq_Z * reg * reg * 
   aval * Int64.int
| Coq_addr_strength_reduction_64_generic_case3 of coq_Z * coq_Z * reg * 
   reg * aval * Int64.int
| Coq_addr_strength_reduction_64_generic_case4 of coq_Z * coq_Z * reg * 
   reg * Int64.int * aval
| Coq_addr_strength_reduction_64_generic_default of addressing * reg list
   * aval list

val addr_strength_reduction_64_generic_match :
  addressing -> reg list -> aval list ->
  addr_strength_reduction_64_generic_cases

val addr_strength_reduction_64_generic :
  addressing -> reg list -> aval list -> addressing * reg list

type addr_strength_reduction_64_cases =
| Coq_addr_strength_reduction_64_case1 of coq_Z * reg * ident * Ptrofs.int
| Coq_addr_strength_reduction_64_case2 of coq_Z * reg * Ptrofs.int
| Coq_addr_strength_reduction_64_case3 of coq_Z * reg * reg * ident
   * Ptrofs.int * Int64.int
| Coq_addr_strength_reduction_64_case4 of coq_Z * reg * reg * Int64.int
   * ident * Ptrofs.int
| Coq_addr_strength_reduction_64_case5 of coq_Z * reg * reg * Ptrofs.int
   * Int64.int
| Coq_addr_strength_reduction_64_case6 of coq_Z * reg * reg * Int64.int
   * Ptrofs.int
| Coq_addr_strength_reduction_64_case7 of coq_Z * coq_Z * reg * reg * 
   ident * Ptrofs.int * Int64.int
| Coq_addr_strength_reduction_64_default of addressing * reg list * aval list

val addr_strength_reduction_64_match :
  addressing -> reg list -> aval list -> addr_strength_reduction_64_cases

val addr_strength_reduction_64 :
  addressing -> reg list -> aval list -> addressing * reg list

val addr_strength_reduction :
  addressing -> reg list -> aval list -> addressing * reg list

val make_addimm : Int.int -> reg -> operation * reg list

val make_shlimm : Int.int -> reg -> reg -> operation * reg list

val make_shrimm : Int.int -> reg -> reg -> operation * reg list

val make_shruimm : Int.int -> reg -> reg -> operation * reg list

val make_mulimm : Int.int -> reg -> operation * reg list

val make_andimm : Int.int -> reg -> aval -> operation * reg list

val make_orimm : Int.int -> reg -> operation * reg list

val make_xorimm : Int.int -> reg -> operation * reg list

val make_divimm : Int.int -> reg -> reg -> operation * reg list

val make_divuimm : Int.int -> reg -> reg -> operation * reg list

val make_moduimm : Int.int -> reg -> reg -> operation * reg list

val make_addlimm : Int64.int -> reg -> operation * reg list

val make_shllimm : Int.int -> reg -> reg -> operation * reg list

val make_shrlimm : Int.int -> reg -> reg -> operation * reg list

val make_shrluimm : Int.int -> reg -> reg -> operation * reg list

val make_mullimm : Int64.int -> reg -> operation * reg list

val make_andlimm : Int64.int -> reg -> aval -> operation * reg list

val make_orlimm : Int64.int -> reg -> operation * reg list

val make_xorlimm : Int64.int -> reg -> operation * reg list

val make_divlimm : Int64.int -> reg -> reg -> operation * reg list

val make_divluimm : Int64.int -> reg -> reg -> operation * reg list

val make_modluimm : Int64.int -> reg -> reg -> operation * reg list

val make_mulfimm : float -> reg -> reg -> reg -> operation * reg list

val make_mulfsimm : float32 -> reg -> reg -> reg -> operation * reg list

val make_cast8signed : reg -> aval -> operation * reg list

val make_cast8unsigned : reg -> aval -> operation * reg list

val make_cast16signed : reg -> aval -> operation * reg list

val make_cast16unsigned : reg -> aval -> operation * reg list

type op_strength_reduction_cases =
| Coq_op_strength_reduction_case1 of reg * aval
| Coq_op_strength_reduction_case2 of reg * aval
| Coq_op_strength_reduction_case3 of reg * aval
| Coq_op_strength_reduction_case4 of reg * aval
| Coq_op_strength_reduction_case5 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case6 of reg * reg * Int.int * aval
| Coq_op_strength_reduction_case7 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case8 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case9 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case10 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case11 of reg * reg * Int.int * aval
| Coq_op_strength_reduction_case12 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case13 of Int.int * reg * aval
| Coq_op_strength_reduction_case14 of reg * reg * Int.int * aval
| Coq_op_strength_reduction_case15 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case16 of reg * reg * Int.int * aval
| Coq_op_strength_reduction_case17 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case18 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case19 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case20 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case21 of addressing * reg list * aval list
| Coq_op_strength_reduction_case22 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case23 of reg * reg * Int64.int * aval
| Coq_op_strength_reduction_case24 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case25 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case26 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case27 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case28 of reg * reg * Int64.int * aval
| Coq_op_strength_reduction_case29 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case30 of Int64.int * reg * aval
| Coq_op_strength_reduction_case31 of reg * reg * Int64.int * aval
| Coq_op_strength_reduction_case32 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case33 of reg * reg * Int64.int * aval
| Coq_op_strength_reduction_case34 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case35 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case36 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case37 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case38 of addressing * reg list * aval list
| Coq_op_strength_reduction_case39 of condition * reg list * aval list
| Coq_op_strength_reduction_case40 of condition * typ * reg * reg * reg list
   * aval * aval * aval list
| Coq_op_strength_reduction_case41 of reg * reg * aval * float
| Coq_op_strength_reduction_case42 of reg * reg * float * aval
| Coq_op_strength_reduction_case43 of reg * reg * aval * float32
| Coq_op_strength_reduction_case44 of reg * reg * float32 * aval
| Coq_op_strength_reduction_default of operation * reg list * aval list

val op_strength_reduction_match :
  operation -> reg list -> aval list -> op_strength_reduction_cases

val op_strength_reduction :
  operation -> reg list -> aval list -> operation * reg list
