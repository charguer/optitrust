open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence.sub_between [tAfter; sInstr "int u"] [tLast; cFunDef "main"; dBody];

  !! Sequence.sub 1 [cVarDef "x"];

  !! Sequence.sub_between [tBefore; cVarDef "y"] [tAfter; cVarDef "t"];

  (* TODO ARTHUR: if user type F6 on the line of the failure, ignore the failure_expected
     command *)

  !! Tools.failure_expected (fun () ->
       Sequence.sub_between [tAfter; cVarDef "z"] [tBefore; cVarDef "z"]);

  !! Tools.failure_expected (fun () ->
       Sequence.sub_between [tAfter; cVarDef "z"] [tAfter; cVarDef "z"]);
)
