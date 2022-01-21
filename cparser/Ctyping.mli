open AST
open Archi
open BinInt
open BinNat
open Cop
open Coqlib
open Csyntax
open Ctypes
open Errors
open Floats
open Integers
open Maps
open Values

val strict : bool

val size_t : coq_type

val ptrdiff_t : coq_type

val attr_add_volatile : bool -> attr -> attr

val type_of_member : attr -> ident -> members -> coq_type res

val type_unop : unary_operation -> coq_type -> coq_type res

val binarith_type : coq_type -> coq_type -> char list -> coq_type res

val binarith_int_type : coq_type -> coq_type -> char list -> coq_type res

val shift_op_type : coq_type -> coq_type -> char list -> coq_type res

val comparison_type : coq_type -> coq_type -> char list -> coq_type res

val type_binop : binary_operation -> coq_type -> coq_type -> coq_type res

val type_deref : coq_type -> coq_type res

val attr_combine : attr -> attr -> attr

val intsize_eq : intsize -> intsize -> bool

val signedness_eq : signedness -> signedness -> bool

val floatsize_eq : floatsize -> floatsize -> bool

val callconv_combine :
  calling_convention -> calling_convention -> calling_convention res

val type_combine : coq_type -> coq_type -> coq_type res

val typelist_combine : typelist -> typelist -> typelist res

val is_void : coq_type -> bool

val type_conditional : coq_type -> coq_type -> coq_type res

type typenv = coq_type PTree.t

val bind_vars : typenv -> (ident * coq_type) list -> typenv

val bind_globdef :
  typenv -> (ident * (Csyntax.fundef, coq_type) globdef) list -> typenv

val check_cast : coq_type -> coq_type -> unit res

val check_bool : coq_type -> unit res

val check_arguments : exprlist -> typelist -> unit res

val check_rval : expr -> unit res

val check_lval : expr -> unit res

val check_rvals : exprlist -> unit res

val evar : typenv -> ident -> expr res

val ederef : expr -> expr res

val efield : composite_env -> expr -> ident -> expr res

val econst_int : Int.int -> signedness -> expr

val econst_ptr_int : Int.int -> coq_type -> expr

val econst_long : Int64.int -> signedness -> expr

val econst_float : float -> expr

val econst_single : float32 -> expr

val evalof : expr -> expr res

val eaddrof : expr -> expr res

val eunop : unary_operation -> expr -> expr res

val ebinop : binary_operation -> expr -> expr -> expr res

val ecast : coq_type -> expr -> expr res

val eseqand : expr -> expr -> expr res

val eseqor : expr -> expr -> expr res

val econdition : expr -> expr -> expr -> expr res

val esizeof : coq_type -> expr

val ealignof : coq_type -> expr

val eassign : expr -> expr -> expr res

val eassignop : binary_operation -> expr -> expr -> expr res

val epostincrdecr : incr_or_decr -> expr -> expr res

val epostincr : expr -> expr res

val epostdecr : expr -> expr res

val epreincr : expr -> expr res

val epredecr : expr -> expr res

val ecomma : expr -> expr -> expr res

val ecall : expr -> exprlist -> expr res

val ebuiltin :
  external_function -> typelist -> exprlist -> coq_type -> expr res

val eselection : expr -> expr -> expr -> expr res

val sdo : expr -> statement res

val sifthenelse : expr -> statement -> statement -> statement res

val swhile : expr -> statement -> statement res

val sdowhile : expr -> statement -> statement res

val sfor : statement -> expr -> statement -> statement -> statement res

val sreturn : coq_type -> expr -> statement res

val sswitch : expr -> labeled_statements -> statement res

val retype_expr : composite_env -> typenv -> expr -> expr res

val retype_exprlist : composite_env -> typenv -> exprlist -> exprlist res

val retype_stmt :
  composite_env -> typenv -> coq_type -> statement -> statement res

val retype_lblstmts :
  composite_env -> typenv -> coq_type -> labeled_statements ->
  labeled_statements res

val retype_function :
  composite_env -> typenv -> coq_function -> coq_function res

val retype_fundef :
  composite_env -> typenv -> Csyntax.fundef -> Csyntax.fundef res

val typecheck_program : Csyntax.program -> Csyntax.program res
