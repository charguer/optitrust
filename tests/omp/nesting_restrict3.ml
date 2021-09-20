open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [cFor "i"];
  !! Omp.parallel [Default Shared_m] [tBefore; cSeq ~args:[[cFor "i"]] ()];
  !! Omp.for_ [] [tBefore; cFor "i"];
  !! Omp.single [] [tBefore; cFun "work"];
)
