open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* LATER/ sExpr->cINstr *)
   !! Sequence.sub_between  [ tAfter; sInstr "int u" ] [ tLast; cFunDef "main"; cStrict; dBody];

  !! Sequence.sub 1 [cVarDef "x"];
  !! Sequence.sub_between [tBefore;cVarDef "y"] [tAfter;cVarDef "t"];

  !! Tools.failure_expected (fun () ->
       Sequence.sub_between  [tAfter;cVarDef "z"] [tBefore;cVarDef "z"]);
  !! Tools.failure_expected (fun () ->
       Sequence.sub_between  [tAfter;cVarDef "z"] [tAfter;cVarDef "z"]);
)
