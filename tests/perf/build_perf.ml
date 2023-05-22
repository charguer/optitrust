open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  Omp.parallel [nbMulti; cFor ""];
)
