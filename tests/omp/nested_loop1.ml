open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [cFor_c "i"];
  !! Sequence_basic.intro 1 [cFor_c "j"];
  !! Omp.parallel [Default Shared_m] [tBefore; cSeq ~args:[cFor_c "i"] ()];
  !! Omp.for_ [] [tBefore; cFor_c "i"];
  !! Omp.parallel [Shared ["i";"n"]] [tBefore; cSeq ~args:[cFor_c "j"] ()];
  !! Omp.for_ [] [tBefore; cFor_c "j"];
)
