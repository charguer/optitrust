open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 
  !! Sequence_basic.intro 3 [cFun "XAXIS"];
  !! Omp.section [tBefore; cFun "XAXIS"];
  !! Omp.section [tBefore; cFun "YAXIS"];
  !! Omp.section [tBefore; cFun "ZAXIS"];
  !! Omp.parallel_sections [] [tBefore;cSeq ~args_pred:(target_list_one_st [cFun "XAXIS"])()];
)