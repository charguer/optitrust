open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [cFor "i"];
  !! Omp.parallel [Default Shared_m] [tBefore; cSeq ~args:[[cFor "i"]] ()];
  !! Omp.for_ [] [tBefore; cFor "i"];
  !! Omp.barrier [tBefore; occIndex ~nb:2 1;cFun "work"];
)
