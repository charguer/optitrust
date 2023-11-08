open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  (* TODO: implement uninit-based checks *)
  (* TODO: add function calls. *)

  (* 1. effects/bindings are unused *)
  !! Sequence_basic.delete [cFunBody "dead_code"; sInstr "a++"];
  !! Sequence_basic.delete [nbMulti; cFunBody "dead_code"; cOr [[cVarDef "a"](* FIXME: ; [cVarDef "v"]*)]];
  (* FIXME:  !! Sequence_basic.delete [nbMulti; cFunBody "dead_code"; sInstr "u."]; *)
  !! Sequence_basic.delete [cFunBody "dead_code"; cVarDef "y"];
  !! Sequence_basic.delete [cFunBody "dead_code"; sInstr "z = i + j"];

  (* 2. effects are shadowed *)
  !! Sequence_basic.delete [cFunBody "shadowed"; sInstr "x = 3"];
  !! Sequence_basic.delete [occFirst; cFunBody "shadowed"; sInstr "*y = x"];

  (* can't delete expression, must be an instruction. *)
  !! Trace.failure_expected (fun () ->
       Sequence_basic.delete [nbMulti; cFunBody "wrong_target"; cInt 8]);

)
