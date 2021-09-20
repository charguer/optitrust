open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cVarDef "p"];
  !! Sequence_basic.intro 1 [cSeq ~args_pred:(Target.target_list_one_st [cVarDef "p"]) ()];
  !! Omp.parallel [] [tFirst; cFunDef "increment_list_items"; dBody];
  !! Omp.single [] [tBefore; cSeq ~args_pred:(Target.target_list_one_st [cVarDef "p"]) ()];
  !! Omp.task [] [tBefore; cFun "process"];

)
