open AST
open Archi
open BinInt
open BinNums
open Cop
open Coqlib
open Csyntax
open Ctypes
open Datatypes
open Errors
open Floats
open Integers
open List0
open Maps
open Memdata
open Memory
open Values

val do_cast : coq_val -> coq_type -> coq_type -> coq_val res

val lookup_composite : composite_env -> ident -> composite res

val constval : composite_env -> expr -> coq_val res

val constval_cast : composite_env -> expr -> coq_type -> coq_val res

type state = { init : init_data list; curr : coq_Z; total_size : coq_Z }

val initial_state : coq_Z -> state

val int_of_byte : Byte.int -> Int.int

val coq_Init_byte : Byte.int -> init_data

val add_rev_bytes : Byte.int list -> init_data list -> init_data list

val add_zeros : coq_Z -> init_data list -> init_data list

val normalize : init_data list -> coq_Z -> init_data list res

val decompose_rec :
  Byte.int list -> init_data list -> coq_Z -> (Byte.int list * init_data
  list) res

val decompose :
  init_data list -> coq_Z -> (Byte.int list * init_data list) res

val trisection :
  init_data list -> coq_Z -> coq_Z -> ((Byte.int list * Byte.int
  list) * init_data list) res

val pad_to : state -> coq_Z -> state

val store_data : state -> coq_Z -> init_data -> state res

val init_data_for_carrier : intsize -> Int.int -> init_data

val store_int : state -> coq_Z -> intsize -> Int.int -> state res

val load_int : state -> coq_Z -> intsize -> Int.int res

val init_data_list_of_state : state -> init_data list res

type coq_initializer =
| Init_single of expr
| Init_array of initializer_list
| Init_struct of initializer_list
| Init_union of ident * coq_initializer
and initializer_list =
| Init_nil
| Init_cons of coq_initializer * initializer_list

val length_initializer_list : initializer_list -> coq_Z

val transl_init_single : composite_env -> coq_type -> expr -> init_data res

val transl_init_bitfield :
  composite_env -> state -> coq_type -> intsize -> coq_Z -> coq_Z ->
  coq_initializer -> coq_Z -> state res

val member_not_initialized : member -> bool

val transl_init_rec :
  composite_env -> state -> coq_type -> coq_initializer -> coq_Z -> state res

val transl_init_array :
  composite_env -> state -> coq_type -> initializer_list -> coq_Z -> state res

val transl_init_struct :
  composite_env -> state -> members -> initializer_list -> coq_Z -> coq_Z ->
  state res

val transl_init :
  composite_env -> coq_type -> coq_initializer -> init_data list res
