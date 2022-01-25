open Alphabet
open Automaton
open Datatypes
open List0

module Make :
 functor (A:T) ->
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
