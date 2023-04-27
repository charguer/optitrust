open Alphabet

type __ = Obj.t

type 'x arrows_right = __

module type T =
 sig
  type terminal

  type nonterminal

  val coq_TerminalAlph : terminal coq_Alphabet

  val coq_NonTerminalAlph : nonterminal coq_Alphabet

  type symbol =
  | T of terminal
  | NT of nonterminal

  val symbol_rect : (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1

  val symbol_rec : (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1

  val coq_SymbolAlph : symbol coq_Alphabet

  type symbol_semantic_type

  type production

  val coq_ProductionAlph : production coq_Alphabet

  val prod_lhs : production -> nonterminal

  val prod_rhs_rev : production -> symbol list

  val prod_action : production -> symbol_semantic_type arrows_right

  type token

  val token_term : token -> terminal

  val token_sem : token -> symbol_semantic_type
 end
