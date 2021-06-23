open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* LATER/ cExpr->cINstr *)
   !! Sequence.sub_between [ cAfter; cExpr "int u" ] [ cLast; cFunDef "main"; cStrict; cBody];

  !! Sequence.sub 1 [cVarDef "x"];
  !! Sequence.sub_between [cBefore;cVarDef "y"] [cAfter;cVarDef "t"];

  !! Tools.failure_expected (fun () ->
       Sequence.sub_between  [cAfter;cVarDef "z"] [cBefore;cVarDef "z"]);
  !! Tools.failure_expected (fun () ->
       Sequence.sub_between  [cAfter;cVarDef "z"] [cAfter;cVarDef "z"]);
)
