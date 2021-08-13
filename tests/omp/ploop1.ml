open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.parallel_for [] [tBefore; cFor_c "i"];
)