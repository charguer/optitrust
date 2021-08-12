open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Sequence_basic.intro 1 [cFor_c "i"];
  !! Sequence_basic.intro 1 [cSeq ~args:[cFor_c "i"] ()];
  !! Omp.parallel [] [tFirst; cFunDef "main";dBody];
  !! Omp.single [] [tBefore;cSeq ~args:[cFor_c "i"] ()];
  !! Omp.task [] [tAfter; cFun "process"]; 
)