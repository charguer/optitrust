open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Transfo_debug.current_ast "TEST";
  !! Resources.show ();

  (* 1. effects/bindings are unused *)
  !! Sequence_basic.delete [cFunBody "dead_code"; sInstr "a++"];
  !! Sequence_basic.delete [nbMulti; cFunBody "dead_code"; cOr [[cVarDef "a"](* FIXME: ; [cVarDef "v"]*)]];
  (* FIXME:  !! Sequence_basic.delete [nbMulti; cFunBody "dead_code"; sInstr "u."]; *)
  !! Sequence_basic.delete [cFunBody "dead_code"; cVarDef "y"];

  (* 2. effects are shadowed *)

  (* can't delete expression, must be an instruction. *)
  !! Trace.failure_expected (fun () ->
       Sequence_basic.delete [nbMulti; cFunBody "wrong_target"; cInt 8]);

)
