open AST
open BinInt
open BinNums
open BinPos
open Clight
open Cop
open Csyntax
open Ctypes
open Datatypes
open Errors
open Integers
open Maps
open Memory
open Values
open Zpower

type generator = { gen_next : ident; gen_trail : (ident * coq_type) list }

type 'a result =
| Err of errmsg
| Res of 'a * generator

type 'a mon = generator -> 'a result

val first_unused_ident : unit -> ident

val initial_generator : unit -> generator

val gensym : coq_type -> ident mon

val makeseq_rec :
  Clight.statement -> Clight.statement list -> Clight.statement

val makeseq : Clight.statement list -> Clight.statement

val eval_simpl_expr : Clight.expr -> coq_val option

val makeif :
  Clight.expr -> Clight.statement -> Clight.statement -> Clight.statement

val coq_Ederef' : Clight.expr -> coq_type -> Clight.expr

val coq_Eaddrof' : Clight.expr -> coq_type -> Clight.expr

val transl_incrdecr : incr_or_decr -> Clight.expr -> coq_type -> Clight.expr

val is_bitfield_access_aux :
  composite_env -> (composite_env -> ident -> members -> (coq_Z * bitfield)
  res) -> ident -> ident -> bitfield mon

val is_bitfield_access : composite_env -> Clight.expr -> bitfield mon

val chunk_for_volatile_type : coq_type -> bitfield -> memory_chunk option

val make_set : bitfield -> ident -> Clight.expr -> Clight.statement

val transl_valof :
  composite_env -> coq_type -> Clight.expr -> (Clight.statement
  list * Clight.expr) mon

val make_assign : bitfield -> Clight.expr -> Clight.expr -> Clight.statement

val make_normalize :
  intsize -> signedness -> coq_Z -> Clight.expr -> Clight.expr

val make_assign_value : bitfield -> Clight.expr -> Clight.expr

type set_destination =
| SDbase of coq_type * coq_type * ident
| SDcons of coq_type * coq_type * ident * set_destination

type destination =
| For_val
| For_effects
| For_set of set_destination

val dummy_expr : Clight.expr

val do_set : set_destination -> Clight.expr -> Clight.statement list

val finish :
  destination -> Clight.statement list -> Clight.expr -> Clight.statement
  list * Clight.expr

val sd_temp : set_destination -> ident

val sd_seqbool_val : ident -> coq_type -> set_destination

val sd_seqbool_set : coq_type -> set_destination -> set_destination

val transl_expr :
  composite_env -> destination -> expr -> (Clight.statement
  list * Clight.expr) mon

val transl_expression :
  composite_env -> expr -> (Clight.statement * Clight.expr) mon

val transl_expr_stmt : composite_env -> expr -> Clight.statement mon

val transl_if :
  composite_env -> expr -> Clight.statement -> Clight.statement ->
  Clight.statement mon

val is_Sskip : statement -> bool

val transl_stmt : composite_env -> statement -> Clight.statement mon

val transl_function : composite_env -> coq_function -> Clight.coq_function res

val transl_fundef : composite_env -> Csyntax.fundef -> Clight.fundef res

val transl_program : Csyntax.program -> Clight.program res
