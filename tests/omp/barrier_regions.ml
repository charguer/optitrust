open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.barrier [tAfter; occIndex ~nb:2 0; cFunDef "sub3"; cCall "work"];
  !! Omp.parallel [Shared ["k"]] [tBefore; cFunDef "sub2";cCall "sub3"];
  !! Omp.parallel [Private ["i"]; Shared ["n"]] [tBefore; cSeq ~args:[[cFor "i"]] ()];
  !! Omp.for_ [] [tBefore; cFor "i"];
)
