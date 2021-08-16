open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic. intro 1 [cFor "i"];
  !! Omp.target [] [tBefore; cFor "i"];  
  !! Omp.parallel_for [] [tBefore; cFor "i"];
)
