open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Sequence.intro 1 [cVarDef "x"];
  !! Sequence.intro_between [tBefore; cVarDef "y"] [tAfter; cVarDef "t"];
  !! Sequence.intro_between [tAfter; sInstr "int u"] [tLast; cFunDef "main"; dBody];

  !! Tools.failure_expected (fun () ->
       Sequence.intro_between [tAfter; cVarDef "z"] [tBefore; cVarDef "z"]);
  !! Tools.failure_expected (fun () ->
       Sequence.intro_between [tAfter; cVarDef "z"] [tAfter; cVarDef "z"]);
)
