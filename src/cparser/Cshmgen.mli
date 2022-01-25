open AST
open Archi
open BinInt
open BinNums
open Clight
open Cminor
open Conventions1
open Cop
open Coqlib
open Csharpminor
open Ctypes
open Datatypes
open Errors
open Floats
open Integers
open List0
open Maps
open Zpower

val make_intconst : Int.int -> expr

val make_longconst : Int64.int -> expr

val make_floatconst : float -> expr

val make_singleconst : float32 -> expr

val make_ptrofsconst : coq_Z -> expr

val make_singleoffloat : expr -> expr

val make_floatofsingle : expr -> expr

val make_floatofint : expr -> signedness -> expr

val make_singleofint : expr -> signedness -> expr

val make_intoffloat : expr -> signedness -> expr

val make_intofsingle : expr -> signedness -> expr

val make_longofint : expr -> signedness -> expr

val make_floatoflong : expr -> signedness -> expr

val make_singleoflong : expr -> signedness -> expr

val make_longoffloat : expr -> signedness -> expr

val make_longofsingle : expr -> signedness -> expr

val make_cmpu_ne_zero : expr -> expr

val sizeof : composite_env -> coq_type -> coq_Z res

val alignof : composite_env -> coq_type -> coq_Z res

val make_cast_int : expr -> intsize -> signedness -> expr

val make_cast : coq_type -> coq_type -> expr -> expr res

val make_boolean : expr -> coq_type -> expr

val make_notbool : expr -> coq_type -> expr res

val make_neg : expr -> coq_type -> expr res

val make_absfloat : expr -> coq_type -> expr res

val make_notint : expr -> coq_type -> expr res

val make_binarith :
  binary_operation -> binary_operation -> binary_operation ->
  binary_operation -> binary_operation -> binary_operation -> expr ->
  coq_type -> expr -> coq_type -> expr res

val make_add_ptr_int :
  composite_env -> coq_type -> signedness -> expr -> expr -> expr res

val make_add_ptr_long : composite_env -> coq_type -> expr -> expr -> expr res

val make_add :
  composite_env -> expr -> coq_type -> expr -> coq_type -> expr res

val make_sub :
  composite_env -> expr -> coq_type -> expr -> coq_type -> expr res

val make_mul : expr -> coq_type -> expr -> coq_type -> expr res

val make_div : expr -> coq_type -> expr -> coq_type -> expr res

val make_binarith_int :
  binary_operation -> binary_operation -> binary_operation ->
  binary_operation -> expr -> coq_type -> expr -> coq_type -> expr res

val make_mod : expr -> coq_type -> expr -> coq_type -> expr res

val make_and : expr -> coq_type -> expr -> coq_type -> expr res

val make_or : expr -> coq_type -> expr -> coq_type -> expr res

val make_xor : expr -> coq_type -> expr -> coq_type -> expr res

val make_shl : expr -> coq_type -> expr -> coq_type -> expr res

val make_shr : expr -> coq_type -> expr -> coq_type -> expr res

val make_cmp_ptr : comparison -> expr -> expr -> expr

val make_cmp : comparison -> expr -> coq_type -> expr -> coq_type -> expr res

val make_extract_bitfield :
  intsize -> signedness -> coq_Z -> coq_Z -> expr -> expr res

val make_load : expr -> coq_type -> bitfield -> expr res

val make_store_bitfield :
  intsize -> signedness -> coq_Z -> coq_Z -> expr -> expr -> stmt res

val make_memcpy : composite_env -> expr -> expr -> coq_type -> stmt res

val make_store :
  composite_env -> expr -> coq_type -> bitfield -> expr -> stmt res

val transl_unop : Cop.unary_operation -> expr -> coq_type -> expr res

val transl_binop :
  composite_env -> Cop.binary_operation -> expr -> coq_type -> expr ->
  coq_type -> expr res

val make_field_access :
  composite_env -> coq_type -> ident -> expr -> (expr * bitfield) res

val transl_expr : composite_env -> Clight.expr -> expr res

val transl_lvalue : composite_env -> Clight.expr -> (expr * bitfield) res

val transl_arglist :
  composite_env -> Clight.expr list -> typelist -> expr list res

val typlist_of_arglist : Clight.expr list -> typelist -> typ list

val make_normalization : coq_type -> expr -> expr

val make_funcall :
  ident option -> coq_type -> signature -> expr -> expr list -> stmt

val transl_statement :
  composite_env -> coq_type -> nat -> nat -> statement -> stmt res

val transl_lbl_stmt :
  composite_env -> coq_type -> nat -> nat -> labeled_statements -> lbl_stmt
  res

val transl_var : composite_env -> (ident * coq_type) -> (ident * coq_Z) res

val signature_of_function : Clight.coq_function -> signature

val transl_function : composite_env -> Clight.coq_function -> coq_function res

val transl_fundef :
  composite_env -> ident -> Clight.fundef -> Csharpminor.fundef res

val transl_globvar : ident -> coq_type -> unit res

val transl_program : Clight.program -> Csharpminor.program res
