open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel_for ~clause:[If "N > THRESHOLD2"] [cFor_c ""];
  
  !! Omp.target ~clause:[If "N > THRESHOLD1"; Map_c (To, ["v1[0:N]"; "v2[:N]"]); Map_c (From, ["p[0:N]"])] [cFor_c ""];

)
