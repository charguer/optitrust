open Archi
open BinInt
open BinNums
open CminorSel
open Datatypes
open Integers
open Op
open SelectOp
open SplitLong

val longconst : Int64.int -> expr

val is_longconst : expr -> Int64.int option

val intoflong : expr -> expr

val longofint : expr -> expr

val longofintu : expr -> expr

type notl_cases =
| Coq_notl_case1 of Int64.int
| Coq_notl_case2 of expr
| Coq_notl_default of expr

val notl_match : expr -> notl_cases

val notl : expr -> expr

type andlimm_cases =
| Coq_andlimm_case1 of Int64.int
| Coq_andlimm_case2 of Int64.int * expr
| Coq_andlimm_default of expr

val andlimm_match : expr -> andlimm_cases

val andlimm : Int64.int -> expr -> expr

type andl_cases =
| Coq_andl_case1 of Int64.int * expr
| Coq_andl_case2 of expr * Int64.int
| Coq_andl_default of expr * expr

val andl_match : expr -> expr -> andl_cases

val andl : expr -> expr -> expr

type orlimm_cases =
| Coq_orlimm_case1 of Int64.int
| Coq_orlimm_case2 of Int64.int * expr
| Coq_orlimm_default of expr

val orlimm_match : expr -> orlimm_cases

val orlimm : Int64.int -> expr -> expr

type orl_cases =
| Coq_orl_case1 of Int64.int * expr
| Coq_orl_case2 of expr * Int64.int
| Coq_orl_case3 of Int.int * expr * Int.int * expr
| Coq_orl_case4 of Int.int * expr * Int.int * expr
| Coq_orl_default of expr * expr

val orl_match : expr -> expr -> orl_cases

val orl : expr -> expr -> expr

type xorlimm_cases =
| Coq_xorlimm_case1 of Int64.int
| Coq_xorlimm_case2 of Int64.int * expr
| Coq_xorlimm_case3 of expr
| Coq_xorlimm_default of expr

val xorlimm_match : expr -> xorlimm_cases

val xorlimm : Int64.int -> expr -> expr

type xorl_cases =
| Coq_xorl_case1 of Int64.int * expr
| Coq_xorl_case2 of expr * Int64.int
| Coq_xorl_default of expr * expr

val xorl_match : expr -> expr -> xorl_cases

val xorl : expr -> expr -> expr

type shllimm_cases =
| Coq_shllimm_case1 of Int64.int
| Coq_shllimm_case2 of Int.int * expr
| Coq_shllimm_case3 of coq_Z * expr
| Coq_shllimm_default of expr

val shllimm_match : expr -> shllimm_cases

val shllimm : helper_functions -> expr -> Int.int -> expr

type shrluimm_cases =
| Coq_shrluimm_case1 of Int64.int
| Coq_shrluimm_case2 of Int.int * expr
| Coq_shrluimm_default of expr

val shrluimm_match : expr -> shrluimm_cases

val shrluimm : helper_functions -> expr -> Int.int -> expr

type shrlimm_cases =
| Coq_shrlimm_case1 of Int64.int
| Coq_shrlimm_case2 of Int.int * expr
| Coq_shrlimm_default of expr

val shrlimm_match : expr -> shrlimm_cases

val shrlimm : helper_functions -> expr -> Int.int -> expr

val shll : helper_functions -> expr -> expr -> expr

val shrl : helper_functions -> expr -> expr -> expr

val shrlu : helper_functions -> expr -> expr -> expr

type addlimm_cases =
| Coq_addlimm_case1 of Int64.int
| Coq_addlimm_case2 of addressing * exprlist
| Coq_addlimm_default of expr

val addlimm_match : expr -> addlimm_cases

val addlimm : Int64.int -> expr -> expr

type addl_cases =
| Coq_addl_case1 of Int64.int * expr
| Coq_addl_case2 of expr * Int64.int
| Coq_addl_case3 of coq_Z * expr * coq_Z * expr
| Coq_addl_case4 of coq_Z * expr * coq_Z * coq_Z * expr
| Coq_addl_case5 of coq_Z * coq_Z * expr * coq_Z * expr
| Coq_addl_case6 of coq_Z * coq_Z * expr * expr
| Coq_addl_case7 of expr * coq_Z * coq_Z * expr
| Coq_addl_case8 of coq_Z * expr * expr
| Coq_addl_case9 of expr * coq_Z * expr
| Coq_addl_default of expr * expr

val addl_match : expr -> expr -> addl_cases

val addl : expr -> expr -> expr

val negl : expr -> expr

type subl_cases =
| Coq_subl_case1 of expr * Int64.int
| Coq_subl_case2 of coq_Z * expr * coq_Z * expr
| Coq_subl_case3 of coq_Z * expr * expr
| Coq_subl_case4 of expr * coq_Z * expr
| Coq_subl_default of expr * expr

val subl_match : expr -> expr -> subl_cases

val subl : expr -> expr -> expr

val mullimm_base : helper_functions -> Int64.int -> expr -> expr

type mullimm_cases =
| Coq_mullimm_case1 of Int64.int
| Coq_mullimm_case2 of coq_Z * expr
| Coq_mullimm_default of expr

val mullimm_match : expr -> mullimm_cases

val mullimm : helper_functions -> Int64.int -> expr -> expr

type mull_cases =
| Coq_mull_case1 of Int64.int * expr
| Coq_mull_case2 of expr * Int64.int
| Coq_mull_default of expr * expr

val mull_match : expr -> expr -> mull_cases

val mull : helper_functions -> expr -> expr -> expr

val mullhu : helper_functions -> expr -> Int64.int -> expr

val mullhs : helper_functions -> expr -> Int64.int -> expr

val shrxlimm : helper_functions -> expr -> Int.int -> expr

val divlu_base : helper_functions -> expr -> expr -> expr

val modlu_base : helper_functions -> expr -> expr -> expr

val divls_base : helper_functions -> expr -> expr -> expr

val modls_base : helper_functions -> expr -> expr -> expr

val cmplu : comparison -> expr -> expr -> expr

val cmpl : comparison -> expr -> expr -> expr

val longoffloat : helper_functions -> expr -> expr

val floatoflong : helper_functions -> expr -> expr

val longofsingle : helper_functions -> expr -> expr

val singleoflong : helper_functions -> expr -> expr
