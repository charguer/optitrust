open Alphabet
open Datatypes
open Grammar

type __ = Obj.t

module type T =
 sig
  module Gram :
   T

  type noninitstate

  val coq_NonInitStateAlph : noninitstate coq_Alphabet

  type initstate

  val coq_InitStateAlph : initstate coq_Alphabet

  val last_symb_of_non_init_state : noninitstate -> Gram.symbol

  type state =
  | Init of initstate
  | Ninit of noninitstate

  val state_rect : (initstate -> 'a1) -> (noninitstate -> 'a1) -> state -> 'a1

  val state_rec : (initstate -> 'a1) -> (noninitstate -> 'a1) -> state -> 'a1

  val coq_StateAlph : state coq_Alphabet

  type lookahead_action =
  | Shift_act of noninitstate
  | Reduce_act of Gram.production
  | Fail_act

  val lookahead_action_rect :
    Gram.terminal -> (noninitstate -> __ -> 'a1) -> (Gram.production -> 'a1)
    -> 'a1 -> lookahead_action -> 'a1

  val lookahead_action_rec :
    Gram.terminal -> (noninitstate -> __ -> 'a1) -> (Gram.production -> 'a1)
    -> 'a1 -> lookahead_action -> 'a1

  type action =
  | Default_reduce_act of Gram.production
  | Lookahead_act of (Gram.terminal -> lookahead_action)

  val action_rect :
    (Gram.production -> 'a1) -> ((Gram.terminal -> lookahead_action) -> 'a1)
    -> action -> 'a1

  val action_rec :
    (Gram.production -> 'a1) -> ((Gram.terminal -> lookahead_action) -> 'a1)
    -> action -> 'a1

  type item = { prod_item : Gram.production; dot_pos_item : nat;
                lookaheads_item : Gram.terminal list }

  val prod_item : item -> Gram.production

  val dot_pos_item : item -> nat

  val lookaheads_item : item -> Gram.terminal list

  module GramDefs :
   sig
    type parse_tree =
    | Terminal_pt of Gram.token
    | Non_terminal_pt of Gram.production * Gram.token list * parse_tree_list
    and parse_tree_list =
    | Nil_ptl
    | Cons_ptl of Gram.symbol list * Gram.token list * parse_tree_list
       * Gram.symbol * Gram.token list * parse_tree

    val parse_tree_rect :
      (Gram.token -> 'a1) -> (Gram.production -> Gram.token list ->
      parse_tree_list -> 'a1) -> Gram.symbol -> Gram.token list -> parse_tree
      -> 'a1

    val parse_tree_rec :
      (Gram.token -> 'a1) -> (Gram.production -> Gram.token list ->
      parse_tree_list -> 'a1) -> Gram.symbol -> Gram.token list -> parse_tree
      -> 'a1

    val parse_tree_list_rect :
      'a1 -> (Gram.symbol list -> Gram.token list -> parse_tree_list -> 'a1
      -> Gram.symbol -> Gram.token list -> parse_tree -> 'a1) -> Gram.symbol
      list -> Gram.token list -> parse_tree_list -> 'a1

    val parse_tree_list_rec :
      'a1 -> (Gram.symbol list -> Gram.token list -> parse_tree_list -> 'a1
      -> Gram.symbol -> Gram.token list -> parse_tree -> 'a1) -> Gram.symbol
      list -> Gram.token list -> parse_tree_list -> 'a1

    val pt_sem :
      Gram.symbol -> Gram.token list -> parse_tree ->
      Gram.symbol_semantic_type

    val ptl_sem :
      Gram.symbol list -> Gram.token list -> parse_tree_list -> 'a1
      arrows_right -> 'a1

    val pt_size : Gram.symbol -> Gram.token list -> parse_tree -> nat

    val ptl_size :
      Gram.symbol list -> Gram.token list -> parse_tree_list -> nat
   end

  val start_nt : initstate -> Gram.nonterminal

  val action_table : state -> action

  val goto_table : state -> Gram.nonterminal -> noninitstate option

  val past_symb_of_non_init_state : noninitstate -> Gram.symbol list

  val past_state_of_non_init_state : noninitstate -> (state -> bool) list

  val items_of_state : state -> item list

  val nullable_nterm : Gram.nonterminal -> bool

  val first_nterm : Gram.nonterminal -> Gram.terminal list
 end
