open Optitrust
open Target 

let _ = Run.script_cpp (fun _ ->
  !! Sequence_basic.intro 1 [cFor_c "i"];
  !! Omp.task [Priority "i"] [tBefore; cFun "compute_array"]; 
  !! Omp.single [] [tBefore; cSeq ~args:[cFor_c "i"] ()];
  !! Omp.parallel [Private ["i"]] [tFirst; cFunDef "compute_matrix"; dBody];
)
