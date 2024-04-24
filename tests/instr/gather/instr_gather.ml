open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Instr.(gather_targets ~dest:(GatherAtLast)) [cVarDef ""]);

  !! Instr.(gather_targets ~dest:(GatherAtFirst)) [cVarDef ""];
  !! Trace.failure_expected (fun _e -> true) (fun _ ->
      Instr.(gather_targets ~dest:(GatherAt [tAfter; cFor "k"])) [cVarDef ""];
     );
  !! Trace.restore_original();
  !! Instr.(gather_targets ~dest:(GatherAt [tBefore; cFor "i"])) [cVarDef ""];

)
