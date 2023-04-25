open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel_for ~clause:[Private ["i"]] [cFor_c ""];
  !! Omp.target [cFor_c ""];

)
