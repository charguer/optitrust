open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel_for [Private ["i"]; Shared ["x";"y";"n"]; Reduction(Plus,["a"]); Reduction(Power,["b"]);Reduction(Min,["c"]); Reduction(Max,["d"]); LastPrivate ["a"]] [tBefore; cFor "i"];
)
