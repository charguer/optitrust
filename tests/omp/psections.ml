open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Sequence_basic.intro 3 [cFun "XAXIS"];
  !! Omp.section [cFun "XAXIS"];
  !! Omp.section [cFun "YAXIS"];
  !! Omp.section [cFun "ZAXIS"];
  !! Omp.parallel_sections [cSeq ~args_pred:(target_list_one_st [cFun "XAXIS"])()];

)
