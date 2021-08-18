open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->
  
  !! Omp.parallel [] [tBefore;cSeq ~args_pred:(Target.target_list_one_st (sInstr "u.n++")) ()];
  !! Omp.atomic (Some Update) [tBefore; sInstr "u.n++"];
  !! Omp.atomic (Some Update) [tBefore; sInstr "u.x +="];
)
