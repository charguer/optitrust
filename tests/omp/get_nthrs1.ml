open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  
  !! Omp_basic.get_num_threads "np" [tBefore; cFor_c ""];
  !! Omp.parallel_for ~clause:[Schedule (Static, "")] [cFor_c ""];

)

