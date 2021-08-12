open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.parallel [] [tFirst; cFunDef "work";dBody];
  !! Omp.task [] [tBefore;cSeq ~args_pred:(Target.target_list_one_st (sInstr "tp++")) ()];
  !! Omp.task [] [tAfter; sInstr "tp++"]; 
)
