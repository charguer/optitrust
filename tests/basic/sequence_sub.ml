open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Sequence.sub 1 [cVarDef "x"];
  !! Sequence.sub_between [tBefore; cVarDef "y"] [tAfter; cVarDef "t"];
  !! Sequence.sub_between [tAfter; sInstr "int u"] [tLast; cFunDef "main"; dBody];

  !! Tools.failure_expected (fun () ->
       Sequence.sub_between [tAfter; cVarDef "z"] [tBefore; cVarDef "z"]);
  !! Tools.failure_expected (fun () ->
       Sequence.sub_between [tAfter; cVarDef "z"] [tAfter; cVarDef "z"]);
)
