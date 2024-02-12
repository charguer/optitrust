open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Instr_basic.move ~dest:[tBefore;cVarDef "x"] [cVarDef "z"];
  !! Instr_basic.move ~dest:[tBefore;cVarDef "y"] [cVarDef "x"];
  !! Instr_basic.move ~dest:[tAfter;cVarDef "z"] [cVarDef "x"];
  !! Instr_basic.move ~dest:[tAfter;cVarDef "y"] [cVarDef "x"];
  !! Instr_basic.move ~dest:[tAfter;cVarDef "x"] [cVarDef "z"];
  !! Instr_basic.move ~dest:[tAfter;cVarDef "x"] [cVarDef "y"];

  let cVarWrite x = cWrite ~lhs:[cVar x] () in
  !! Instr_basic.move ~dest:[tBefore; cVarDef "t"] [cVarWrite "z"];
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Instr_basic.move ~dest:[tBefore; cVarDef "t"] [cVarWrite "t"]);
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Instr_basic.move ~dest:[tBefore; cVarWrite "x"] [cVarWrite "z"]);
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Instr_basic.move ~dest:[tBefore; cVarWrite "x"] [cVarWrite "z"]);

  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Instr_basic.move ~dest:[tBefore; cVarDef "v"] [cVarWrite "y"]);

  !! Instr_basic.move ~dest:[tLast] [tSpan [tBefore; cVarDef "t"] [tAfter; cVarWrite "t"]];
  !! Instr_basic.move ~dest:[tAfter; cVarWrite "x"] [tSpan [tBefore; cVarDef "v"] [tAfter; cVarWrite "y"]];

  !! Trace.failure_expected (fun _ -> true) (fun () -> Instr_basic.move ~dest:[tAfter; cVarDef "y"] [tSpan [tFirst] [tAfter; cVarWrite "y"]]);
  !! Trace.failure_expected (fun _ -> true) (fun () -> Instr_basic.move ~dest:[tAfter; cVarDef "y"] [tSpan [tBefore; cVarDef "v"] [tAfter; cVarWrite "y"]]);
)
