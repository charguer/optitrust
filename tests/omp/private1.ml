open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [Private ["i"]; FirstPrivate ["j"]] [tAfter; sInstr "ptr_j ="];
)
