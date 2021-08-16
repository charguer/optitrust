open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.init_lock "lock" [tAfter; cVarDef "lock"];
  !! Omp.parallel [] [tBefore; cSeq ~args:[cFor "i"] ()];
  !! Omp.for_ [] [tBefore;cFor "i"];
  !! Omp.task [] [tFirst; cFor "i"; dBody];
  !! Omp.set_lock "lock" [tBefore; cSeq ~args:[cFun "printf"] ()];
  !! Omp.task [] [tBefore; cSeq ~args:[cFun "printf"] ()];
  !! Omp.unset_lock "lock" [tAfter; cSeq ~args:[cFun "printf"] ()];
  !! Omp.task [] [tFirst; cFunDef "work";dBody];
  !! Omp.destroy_lock "lock" [tLast; cFunDef "work"; dBody];
  

)
