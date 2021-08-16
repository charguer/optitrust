open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [Reduction (Plus, ["a[0:N]"]); Private ["j"]] [tBefore; cFor "i"];
)
