open Optitrust
open Target
open Syntax

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel_for ~collapse:2 [nbMulti; cFor "a"];

)
