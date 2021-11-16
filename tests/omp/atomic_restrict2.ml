open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [] [tBefore;cSeq ~args_pred:(Target.target_list_one_st [sInstr "+= 1"] ) ()];
  !! Omp.atomic (Some Update) [tBefore; occIndex ~nb:2 0;sInstr "+="];
  !! Omp.atomic (Some Update) [tBefore; occIndex ~nb:2 1;sInstr "+="];
)
