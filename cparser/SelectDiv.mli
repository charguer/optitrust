open BinInt
open BinNums
open CminorSel
open Compopts
open Coqlib
open Datatypes
open Floats
open Integers
open Op
open SelectLong
open SelectOp
open SplitLong
open Zpower

val is_intconst : expr -> Int.int option

val find_div_mul_params :
  nat -> coq_Z -> coq_Z -> coq_Z -> (coq_Z * coq_Z) option

val divs_mul_params : coq_Z -> (coq_Z * coq_Z) option

val divu_mul_params : coq_Z -> (coq_Z * coq_Z) option

val divls_mul_params : coq_Z -> (coq_Z * coq_Z) option

val divlu_mul_params : coq_Z -> (coq_Z * coq_Z) option

val divu_mul : coq_Z -> coq_Z -> expr

val divuimm : expr -> Int.int -> expr

val divu : expr -> expr -> expr

val mod_from_div : expr -> Int.int -> expr

val moduimm : expr -> Int.int -> expr

val modu : expr -> expr -> expr

val divs_mul : coq_Z -> coq_Z -> expr

val divsimm : expr -> Int.int -> expr

val divs : expr -> expr -> expr

val modsimm : expr -> Int.int -> expr

val mods : expr -> expr -> expr

val modl_from_divl : helper_functions -> expr -> Int64.int -> expr

val divlu_mull : helper_functions -> coq_Z -> coq_Z -> expr

val divlu : helper_functions -> expr -> expr -> expr

val modlu : helper_functions -> expr -> expr -> expr

val divls_mull : helper_functions -> coq_Z -> coq_Z -> expr

val divls : helper_functions -> expr -> expr -> expr

val modls : helper_functions -> expr -> expr -> expr

val divfimm : expr -> float -> expr

type divf_cases =
| Coq_divf_case1 of float
| Coq_divf_default of expr

val divf_match : expr -> divf_cases

val divf : expr -> expr -> expr

val divfsimm : expr -> float32 -> expr

type divfs_cases =
| Coq_divfs_case1 of float32
| Coq_divfs_default of expr

val divfs_match : expr -> divfs_cases

val divfs : expr -> expr -> expr
