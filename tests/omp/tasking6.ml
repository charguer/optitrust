open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Sequence_basic.intro 1 [cFor "i"];
  !! Sequence_basic.intro 1 [cSeq ~args:[cFor "i"] ()];
  !! Sequence_basic.intro 1 [cSeq ~args:[cSeq ~args:[cFor "i"] ()] ()];
  !! Omp.parallel [] [tFirst; cFunDef "main";dBody];
  !! Omp.single [] [tBefore;cSeq ~args:[cSeq ~args:[cFor "i"] ()] ()];
  !! Omp.task [Untied] [tBefore;cSeq ~args:[cFor "i"] ()];
  !! Omp.task [] [tBefore; cFun "process"]; 
)
