open AST
open Archi
open BinInt
open BinNums
open Builtins1
open CminorSel
open Datatypes
open Floats
open Integers
open Op

val symbol_is_external : ident -> bool

val coq_Olea_ptr : addressing -> operation

val addrsymbol : ident -> Ptrofs.int -> expr

val addrstack : Ptrofs.int -> expr

type notint_cases =
| Coq_notint_case1 of Int.int
| Coq_notint_case2 of Int.int * expr
| Coq_notint_default of expr

val notint_match : expr -> notint_cases

val notint : expr -> expr

type addimm_cases =
| Coq_addimm_case1 of Int.int
| Coq_addimm_case2 of addressing * exprlist
| Coq_addimm_default of expr

val addimm_match : expr -> addimm_cases

val addimm : Int.int -> expr -> expr

type add_cases =
| Coq_add_case1 of Int.int * expr
| Coq_add_case2 of expr * Int.int
| Coq_add_case3 of coq_Z * expr * coq_Z * expr
| Coq_add_case4 of coq_Z * expr * coq_Z * coq_Z * expr
| Coq_add_case5 of coq_Z * coq_Z * expr * coq_Z * expr
| Coq_add_case6 of coq_Z * expr * ident * Ptrofs.int
| Coq_add_case7 of ident * Ptrofs.int * coq_Z * expr
| Coq_add_case8 of coq_Z * coq_Z * expr * ident * Ptrofs.int
| Coq_add_case9 of ident * Ptrofs.int * coq_Z * coq_Z * expr
| Coq_add_case10 of coq_Z * coq_Z * expr * expr
| Coq_add_case11 of expr * coq_Z * coq_Z * expr
| Coq_add_case12 of coq_Z * expr * expr
| Coq_add_case13 of expr * coq_Z * expr
| Coq_add_default of expr * expr

val add_match : expr -> expr -> add_cases

val add : expr -> expr -> expr

type negint_cases =
| Coq_negint_case1 of Int.int
| Coq_negint_default of expr

val negint_match : expr -> negint_cases

val negint : expr -> expr

type sub_cases =
| Coq_sub_case1 of expr * Int.int
| Coq_sub_case2 of coq_Z * expr * coq_Z * expr
| Coq_sub_case3 of coq_Z * expr * expr
| Coq_sub_case4 of expr * coq_Z * expr
| Coq_sub_default of expr * expr

val sub_match : expr -> expr -> sub_cases

val sub : expr -> expr -> expr

val shift_is_scale : Int.int -> bool

type shlimm_cases =
| Coq_shlimm_case1 of Int.int
| Coq_shlimm_case2 of Int.int * expr
| Coq_shlimm_case3 of coq_Z * expr
| Coq_shlimm_default of expr

val shlimm_match : expr -> shlimm_cases

val shlimm : expr -> Int.int -> expr

type shruimm_cases =
| Coq_shruimm_case1 of Int.int
| Coq_shruimm_case2 of Int.int * expr
| Coq_shruimm_default of expr

val shruimm_match : expr -> shruimm_cases

val shruimm : expr -> Int.int -> expr

type shrimm_cases =
| Coq_shrimm_case1 of Int.int
| Coq_shrimm_case2 of Int.int * expr
| Coq_shrimm_default of expr

val shrimm_match : expr -> shrimm_cases

val shrimm : expr -> Int.int -> expr

val mulimm_base : Int.int -> expr -> expr

type mulimm_cases =
| Coq_mulimm_case1 of Int.int
| Coq_mulimm_case2 of coq_Z * expr
| Coq_mulimm_default of expr

val mulimm_match : expr -> mulimm_cases

val mulimm : Int.int -> expr -> expr

type mul_cases =
| Coq_mul_case1 of Int.int * expr
| Coq_mul_case2 of expr * Int.int
| Coq_mul_default of expr * expr

val mul_match : expr -> expr -> mul_cases

val mul : expr -> expr -> expr

val mulhs : expr -> expr -> expr

val mulhu : expr -> expr -> expr

type andimm_cases =
| Coq_andimm_case1 of Int.int
| Coq_andimm_case2 of Int.int * expr
| Coq_andimm_case3 of expr
| Coq_andimm_case4 of expr
| Coq_andimm_default of expr

val andimm_match : expr -> andimm_cases

val andimm : Int.int -> expr -> expr

type and_cases =
| Coq_and_case1 of Int.int * expr
| Coq_and_case2 of expr * Int.int
| Coq_and_default of expr * expr

val and_match : expr -> expr -> and_cases

val coq_and : expr -> expr -> expr

type orimm_cases =
| Coq_orimm_case1 of Int.int
| Coq_orimm_case2 of Int.int * expr
| Coq_orimm_default of expr

val orimm_match : expr -> orimm_cases

val orimm : Int.int -> expr -> expr

val same_expr_pure : expr -> expr -> bool

type or_cases =
| Coq_or_case1 of Int.int * expr
| Coq_or_case2 of expr * Int.int
| Coq_or_case3 of Int.int * expr * Int.int * expr
| Coq_or_case4 of Int.int * expr * Int.int * expr
| Coq_or_default of expr * expr

val or_match : expr -> expr -> or_cases

val coq_or : expr -> expr -> expr

type xorimm_cases =
| Coq_xorimm_case1 of Int.int
| Coq_xorimm_case2 of Int.int * expr
| Coq_xorimm_case3 of expr
| Coq_xorimm_default of expr

val xorimm_match : expr -> xorimm_cases

val xorimm : Int.int -> expr -> expr

type xor_cases =
| Coq_xor_case1 of Int.int * expr
| Coq_xor_case2 of expr * Int.int
| Coq_xor_default of expr * expr

val xor_match : expr -> expr -> xor_cases

val xor : expr -> expr -> expr

val divu_base : expr -> expr -> expr

val modu_base : expr -> expr -> expr

val divs_base : expr -> expr -> expr

val mods_base : expr -> expr -> expr

val shrximm : expr -> Int.int -> expr

type shl_cases =
| Coq_shl_case1 of Int.int
| Coq_shl_default of expr

val shl_match : expr -> shl_cases

val shl : expr -> expr -> expr

type shr_cases =
| Coq_shr_case1 of Int.int
| Coq_shr_default of expr

val shr_match : expr -> shr_cases

val shr : expr -> expr -> expr

type shru_cases =
| Coq_shru_case1 of Int.int
| Coq_shru_default of expr

val shru_match : expr -> shru_cases

val shru : expr -> expr -> expr

val negf : expr -> expr

val absf : expr -> expr

val addf : expr -> expr -> expr

val subf : expr -> expr -> expr

val mulf : expr -> expr -> expr

val negfs : expr -> expr

val absfs : expr -> expr

val addfs : expr -> expr -> expr

val subfs : expr -> expr -> expr

val mulfs : expr -> expr -> expr

type compimm_cases =
| Coq_compimm_case1 of comparison * Int.int
| Coq_compimm_case2 of condition * exprlist
| Coq_compimm_case3 of condition * exprlist
| Coq_compimm_case4 of Int.int * expr
| Coq_compimm_case5 of Int.int * expr
| Coq_compimm_default of comparison * expr

val compimm_match : comparison -> expr -> compimm_cases

val compimm :
  (comparison -> Int.int -> condition) -> (comparison -> Int.int -> Int.int
  -> bool) -> comparison -> expr -> Int.int -> expr

type comp_cases =
| Coq_comp_case1 of Int.int * expr
| Coq_comp_case2 of expr * Int.int
| Coq_comp_default of expr * expr

val comp_match : expr -> expr -> comp_cases

val comp : comparison -> expr -> expr -> expr

type compu_cases =
| Coq_compu_case1 of Int.int * expr
| Coq_compu_case2 of expr * Int.int
| Coq_compu_default of expr * expr

val compu_match : expr -> expr -> compu_cases

val compu : comparison -> expr -> expr -> expr

val compf : comparison -> expr -> expr -> expr

val compfs : comparison -> expr -> expr -> expr

type cast8unsigned_cases =
| Coq_cast8unsigned_case1 of Int.int
| Coq_cast8unsigned_case2 of Int.int * expr
| Coq_cast8unsigned_default of expr

val cast8unsigned_match : expr -> cast8unsigned_cases

val cast8unsigned : expr -> expr

type cast8signed_cases =
| Coq_cast8signed_case1 of Int.int
| Coq_cast8signed_default of expr

val cast8signed_match : expr -> cast8signed_cases

val cast8signed : expr -> expr

type cast16unsigned_cases =
| Coq_cast16unsigned_case1 of Int.int
| Coq_cast16unsigned_case2 of Int.int * expr
| Coq_cast16unsigned_default of expr

val cast16unsigned_match : expr -> cast16unsigned_cases

val cast16unsigned : expr -> expr

type cast16signed_cases =
| Coq_cast16signed_case1 of Int.int
| Coq_cast16signed_default of expr

val cast16signed_match : expr -> cast16signed_cases

val cast16signed : expr -> expr

val select_supported : typ -> bool

val select_swap : condition -> bool

val select : typ -> condition -> exprlist -> expr -> expr -> expr option

val singleoffloat : expr -> expr

val floatofsingle : expr -> expr

val intoffloat : expr -> expr

type floatofint_cases =
| Coq_floatofint_case1 of Int.int
| Coq_floatofint_default of expr

val floatofint_match : expr -> floatofint_cases

val floatofint : expr -> expr

val intuoffloat : expr -> expr

type floatofintu_cases =
| Coq_floatofintu_case1 of Int.int
| Coq_floatofintu_default of expr

val floatofintu_match : expr -> floatofintu_cases

val floatofintu : expr -> expr

val intofsingle : expr -> expr

type singleofint_cases =
| Coq_singleofint_case1 of Int.int
| Coq_singleofint_default of expr

val singleofint_match : expr -> singleofint_cases

val singleofint : expr -> expr

val intuofsingle : expr -> expr

type singleofintu_cases =
| Coq_singleofintu_case1 of Int.int
| Coq_singleofintu_default of expr

val singleofintu_match : expr -> singleofintu_cases

val singleofintu : expr -> expr

type addressing_cases =
| Coq_addressing_case1 of addressing * exprlist
| Coq_addressing_case2 of addressing * exprlist
| Coq_addressing_default of expr

val addressing_match : expr -> addressing_cases

val addressing : memory_chunk -> expr -> addressing * exprlist

type builtin_arg_addr_cases =
| Coq_builtin_arg_addr_case1 of coq_Z * expr
| Coq_builtin_arg_addr_case2 of ident * Ptrofs.int
| Coq_builtin_arg_addr_case3 of Ptrofs.int
| Coq_builtin_arg_addr_default of addressing * exprlist

val builtin_arg_addr_match : addressing -> exprlist -> builtin_arg_addr_cases

val builtin_arg_addr : addressing -> exprlist -> expr builtin_arg

type builtin_arg_cases =
| Coq_builtin_arg_case1 of Int.int
| Coq_builtin_arg_case2 of Int64.int
| Coq_builtin_arg_case3 of addressing * exprlist
| Coq_builtin_arg_case4 of addressing * exprlist
| Coq_builtin_arg_case5 of Int.int * Int.int
| Coq_builtin_arg_case6 of expr * expr
| Coq_builtin_arg_case7 of memory_chunk * ident * Ptrofs.int
| Coq_builtin_arg_case8 of memory_chunk * Ptrofs.int
| Coq_builtin_arg_default of expr

val builtin_arg_match : expr -> builtin_arg_cases

val builtin_arg : expr -> expr builtin_arg

val platform_builtin : platform_builtin -> exprlist -> expr option
