open Alphabet
open Automaton
open Datatypes
open Grammar
open Int0
open Interpreter
open Interpreter_complete
open Interpreter_correct
open Specif

type __ = Obj.t

module Make =
 functor (Aut:Automaton.T) ->
 struct
  module Inter = Interpreter.Make(Aut)

  module Correct = Make(Aut)(Inter)

  module Complete = Interpreter_complete.Make(Aut)(Inter)

  (** val complete_validator : unit -> bool **)

  let complete_validator =
    Complete.Valid.is_complete

  (** val safe_validator : unit -> bool **)

  let safe_validator =
    Inter.ValidSafe.is_safe

  (** val parse :
      Aut.initstate -> nat -> Inter.buffer -> Aut.Gram.symbol_semantic_type
      Inter.parse_result **)

  let parse init log_n_steps buffer0 =
    Inter.parse init buffer0 log_n_steps
 end
