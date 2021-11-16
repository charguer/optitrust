open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.task [] [tFirst; cFunDef "work";dBody];
  
  !! Omp.task [] [tBefore;occIndex ~nb:2 0;cSeq ~args:[[cSeq ~args:[[cFun "printf"]] ()]] ()];
  !! Omp.critical "" [tBefore;occIndex ~nb:2 0;cSeq ~args:[[cFun "printf"]] ()];

  !! Omp.task [] [tBefore;cSeq ~args:[[cSeq ~args:[[cFun "printf"]] ()]] ()];
  !! Omp.critical "" [tBefore;occIndex ~nb:2 1;cSeq ~args:[[cFun "printf"]] ()];

)
