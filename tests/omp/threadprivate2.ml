open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.threadprivate ["counter"] [tBefore; sInstr "counter++"];
)
