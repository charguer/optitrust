open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 
  !! Omp.barrier [tAfter; tIndex ~nb:2 0; cFunDef "sub3"; cFun "work"];
  !! Omp.parallel [Shared_c ["k"]] [tBefore; cFunDef "sub2";cFun "sub3"];
  !! Omp.parallel [Private ["i"]; Shared_c ["n"]] [tBefore; cSeq ~args:[cFor_c "i"] ()];
  !! Omp.for_ [] [tBefore; cFor_c "i"];
)