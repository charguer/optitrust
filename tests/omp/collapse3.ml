open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Sequence_basic.intro 1 [cFor "k"];
  !! Omp.parallel [Num_threads "2"] [tAfter;cVarDef "a"];
  !! Omp.for_ [Collapse 2; Ordered_c 0;Private ["j"; "k"]; Schedule (Static, "3")] [tBefore;cFor "k"];
  !! Omp.ordered [] [tBefore;cFun "printf"];
)