open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.task [] [tFirst; cFunDef "work";dBody];
  !! Omp.task [] [tBefore;cSeq ~args_pred:(Target.target_list_one_st [sInstr "tp = 1"]) ()];
  !! Omp.task [] [tAfter; sInstr "tp = 1"];
)
