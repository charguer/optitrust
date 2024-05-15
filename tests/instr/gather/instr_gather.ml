open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  (* TODO: flag to prevent moving deps
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Instr.(gather_targets ~dest:(GatherAtLast)) [cFunBody "foo"; cVarDef ""]);
    *)

  !! Instr.(gather_targets ~dest:(GatherAtFirst)) [cFunBody "foo"; cVarDef ""];
  (* FIXME:
  !! Trace.failure_expected (fun _e -> true) (fun _ ->
      Instr.(gather_targets ~dest:(GatherAt [tAfter; cFor "k"])) [cFunBody "foo"; cVarDef ""];
     );
  !! Trace.restore_original();
  !! Instr.(gather_targets ~dest:(GatherAt [tBefore; cFor "i"])) [cFunBody "foo"; cVarDef ""];
*)

  !! Instr.(gather_targets ~dest:(GatherAtLast)) [cFunBody "with_deps"; cPlusEq ~lhs:[cOr [[cVar "a"]; [cVar "b"]]] ()];
)
