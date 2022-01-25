open AST
open Coqlib
open Datatypes
open Floats
open Integers
open List0
open Memdata
open String0
open Values

type __ = Obj.t

type builtin_sem =
  coq_val list -> coq_val option
  (* singleton inductive, whose constructor was mkbuiltin *)

val bs_sem : rettype -> builtin_sem -> coq_val list -> coq_val option

val mkbuiltin_v1t : rettype -> (coq_val -> coq_val) -> builtin_sem

val mkbuiltin_v2t : rettype -> (coq_val -> coq_val -> coq_val) -> builtin_sem

val mkbuiltin_v1p : rettype -> (coq_val -> coq_val option) -> builtin_sem

val mkbuiltin_v2p :
  rettype -> (coq_val -> coq_val -> coq_val option) -> builtin_sem

type valty = __

type valretty = __

val inj_num : rettype -> valretty -> coq_val

val proj_num : typ -> 'a1 -> coq_val -> (valty -> 'a1) -> 'a1

val mkbuiltin_n1t : typ -> rettype -> (valty -> valretty) -> builtin_sem

val mkbuiltin_n2t :
  typ -> typ -> rettype -> (valty -> valty -> valretty) -> builtin_sem

val mkbuiltin_n1p :
  typ -> rettype -> (valty -> valretty option) -> builtin_sem

val lookup_builtin :
  ('a1 -> signature) -> char list -> signature -> (char list * 'a1) list ->
  'a1 option

type standard_builtin =
| BI_select of typ
| BI_fabs
| BI_fabsf
| BI_fsqrt
| BI_negl
| BI_addl
| BI_subl
| BI_mull
| BI_i16_bswap
| BI_i32_bswap
| BI_i64_bswap
| BI_unreachable
| BI_i64_umulh
| BI_i64_smulh
| BI_i64_sdiv
| BI_i64_udiv
| BI_i64_smod
| BI_i64_umod
| BI_i64_shl
| BI_i64_shr
| BI_i64_sar
| BI_i64_dtos
| BI_i64_dtou
| BI_i64_stod
| BI_i64_utod
| BI_i64_stof
| BI_i64_utof

val standard_builtin_table : (char list * standard_builtin) list

val standard_builtin_sig : standard_builtin -> signature

val standard_builtin_sem : standard_builtin -> builtin_sem
