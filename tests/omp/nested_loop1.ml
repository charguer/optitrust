open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [cFor "i"];
  !! Sequence_basic.intro 1 [cFor "j"];
  !! Omp.parallel [Default Shared_m] [tBefore; cSeq ~args:[[cFor "i"]] ()];
  !! Omp.for_ [] [tBefore; cFor "i"];
  !! Omp.parallel [Shared ["i";"n"]] [tBefore; cSeq ~args:[[cFor "j"]] ()];
  !! Omp.for_ [] [tBefore; cFor "j"];
)
