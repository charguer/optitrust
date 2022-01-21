open Alphabet
open Automaton
open Datatypes
open Grammar
open List0
open Specif
open Validator_safe

module Make :
 functor (A:Automaton.T) ->
 sig
  module ValidSafe :
   sig
    val singleton_state_pred : A.state -> A.state -> bool

    val past_state_of_state : A.state -> (A.state -> bool) list

    val head_symbs_of_state : A.state -> A.Gram.symbol list

    val head_states_of_state : A.state -> (A.state -> bool) list

    val is_prefix : A.Gram.symbol list -> A.Gram.symbol list -> bool

    val is_prefix_pred :
      (A.state -> bool) list -> (A.state -> bool) list -> bool

    val is_state_valid_after_pop :
      A.state -> A.Gram.symbol list -> (A.state -> bool) list -> bool

    val is_safe : unit -> bool
   end

  type coq_Decidable = bool

  val decide : coq_Decidable -> bool

  val comparable_decidable_eq :
    'a1 coq_Comparable -> 'a1 -> 'a1 -> coq_Decidable

  val list_decidable_eq :
    ('a1 -> 'a1 -> coq_Decidable) -> 'a1 list -> 'a1 list -> coq_Decidable

  val cast : 'a1 -> 'a1 -> (unit -> coq_Decidable) -> 'a2 -> 'a2

  type buffer = __buffer Lazy.t
  and __buffer =
  | Buf_cons of A.Gram.token * buffer

  val buf_head : buffer -> A.Gram.token

  val buf_tail : buffer -> buffer

  val app_buf : A.Gram.token list -> buffer -> buffer

  type noninitstate_type = A.Gram.symbol_semantic_type

  type stack = (A.noninitstate, noninitstate_type) sigT list

  val state_of_stack : A.initstate -> stack -> A.state

  val state_stack_of_stack : A.initstate -> stack -> (A.state -> bool) list

  val symb_stack_of_stack : stack -> A.Gram.symbol list

  val pop : A.Gram.symbol list -> stack -> 'a1 arrows_right -> stack * 'a1

  type step_result =
  | Fail_sr_full of A.state * A.Gram.token
  | Accept_sr of A.Gram.symbol_semantic_type * buffer
  | Progress_sr of stack * buffer

  val step_result_rect :
    A.initstate -> (A.state -> A.Gram.token -> 'a1) ->
    (A.Gram.symbol_semantic_type -> buffer -> 'a1) -> (stack -> buffer ->
    'a1) -> step_result -> 'a1

  val step_result_rec :
    A.initstate -> (A.state -> A.Gram.token -> 'a1) ->
    (A.Gram.symbol_semantic_type -> buffer -> 'a1) -> (stack -> buffer ->
    'a1) -> step_result -> 'a1

  val reduce_step :
    A.initstate -> stack -> A.Gram.production -> buffer -> step_result

  val step : A.initstate -> stack -> buffer -> step_result

  val parse_fix : A.initstate -> stack -> buffer -> nat -> step_result

  type 'a parse_result =
  | Fail_pr_full of A.state * A.Gram.token
  | Timeout_pr
  | Parsed_pr of 'a * buffer

  val parse_result_rect :
    (A.state -> A.Gram.token -> 'a2) -> 'a2 -> ('a1 -> buffer -> 'a2) -> 'a1
    parse_result -> 'a2

  val parse_result_rec :
    (A.state -> A.Gram.token -> 'a2) -> 'a2 -> ('a1 -> buffer -> 'a2) -> 'a1
    parse_result -> 'a2

  val parse :
    A.initstate -> buffer -> nat -> A.Gram.symbol_semantic_type parse_result
 end
