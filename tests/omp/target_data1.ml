open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic. intro 1 [cFor_c "i"];
  !! Omp.target [] [tBefore; cFor_c "i"];  
  !! Omp.parallel_for [] [tBefore; cFor_c "i"];
)
