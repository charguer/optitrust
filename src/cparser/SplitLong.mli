open AST
open BinNums
open CminorSel
open Datatypes
open Integers
open Op
open SelectOp

type helper_functions = { i64_dtos : ident; i64_dtou : ident;
                          i64_stod : ident; i64_utod : ident;
                          i64_stof : ident; i64_utof : ident;
                          i64_sdiv : ident; i64_udiv : ident;
                          i64_smod : ident; i64_umod : ident;
                          i64_shl : ident; i64_shr : ident; i64_sar : 
                          ident; i64_umulh : ident; i64_smulh : ident }

val sig_l_l : signature

val sig_l_f : signature

val sig_l_s : signature

val sig_f_l : signature

val sig_ll_l : signature

val sig_li_l : signature

val sig_ii_l : signature

val makelong : expr -> expr -> expr

type splitlong_cases =
| Coq_splitlong_case1 of expr * expr
| Coq_splitlong_default of expr

val splitlong_match : expr -> splitlong_cases

val splitlong : expr -> (expr -> expr -> expr) -> expr

type splitlong2_cases =
| Coq_splitlong2_case1 of expr * expr * expr * expr
| Coq_splitlong2_case2 of expr * expr * expr
| Coq_splitlong2_case3 of expr * expr * expr
| Coq_splitlong2_default of expr * expr

val splitlong2_match : expr -> expr -> splitlong2_cases

val splitlong2 :
  expr -> expr -> (expr -> expr -> expr -> expr -> expr) -> expr

type lowlong_cases =
| Coq_lowlong_case1 of expr * expr
| Coq_lowlong_default of expr

val lowlong_match : expr -> lowlong_cases

val lowlong : expr -> expr

type highlong_cases =
| Coq_highlong_case1 of expr * expr
| Coq_highlong_default of expr

val highlong_match : expr -> highlong_cases

val highlong : expr -> expr

val longconst : Int64.int -> expr

type is_longconst_cases =
| Coq_is_longconst_case1 of Int.int * Int.int
| Coq_is_longconst_default of expr

val is_longconst_match : expr -> is_longconst_cases

val is_longconst : expr -> Int64.int option

val is_longconst_zero : expr -> bool

val intoflong : expr -> expr

type longofint_cases =
| Coq_longofint_case1 of Int.int
| Coq_longofint_default of expr

val longofint_match : expr -> longofint_cases

val longofint : expr -> expr

val longofintu : expr -> expr

val negl : expr -> expr

val notl : expr -> expr

val longoffloat : helper_functions -> expr -> expr

val longuoffloat : helper_functions -> expr -> expr

val floatoflong : helper_functions -> expr -> expr

val floatoflongu : helper_functions -> expr -> expr

val longofsingle : helper_functions -> expr -> expr

val longuofsingle : helper_functions -> expr -> expr

val singleoflong : helper_functions -> expr -> expr

val singleoflongu : helper_functions -> expr -> expr

val andl : expr -> expr -> expr

val orl : expr -> expr -> expr

val xorl : expr -> expr -> expr

val shllimm : helper_functions -> expr -> Int.int -> expr

val shrluimm : helper_functions -> expr -> Int.int -> expr

val shrlimm : helper_functions -> expr -> Int.int -> expr

val is_intconst : expr -> Int.int option

val shll : helper_functions -> expr -> expr -> expr

val shrlu : helper_functions -> expr -> expr -> expr

val shrl : helper_functions -> expr -> expr -> expr

val addl : expr -> expr -> expr

val subl : expr -> expr -> expr

val mull_base : expr -> expr -> expr

val mullimm : helper_functions -> Int64.int -> expr -> expr

val mull : helper_functions -> expr -> expr -> expr

val mullhu : helper_functions -> expr -> Int64.int -> expr

val mullhs : helper_functions -> expr -> Int64.int -> expr

val shrxlimm : helper_functions -> expr -> Int.int -> expr

val divlu_base : helper_functions -> expr -> expr -> expr

val modlu_base : helper_functions -> expr -> expr -> expr

val divls_base : helper_functions -> expr -> expr -> expr

val modls_base : helper_functions -> expr -> expr -> expr

val cmpl_eq_zero : expr -> expr

val cmpl_ne_zero : expr -> expr

val cmplu_gen : comparison -> comparison -> expr -> expr -> expr

val cmplu : comparison -> expr -> expr -> expr

val cmpl_gen : comparison -> comparison -> expr -> expr -> expr

val cmpl : comparison -> expr -> expr -> expr
