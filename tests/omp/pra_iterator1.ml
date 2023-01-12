open Optitrust
open Target


let _ = Run.script_cpp (fun _ -> 


  !! Omp.parallel_for ~clause:[Default None_; Shared ["vec"]] [cFor_c ""];

)
