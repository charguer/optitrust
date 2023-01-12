open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel_for [cFor_c ""];
  !! Omp.target ~clause:[Map_c (No_map,["v1"; "v2"; "p"])] [cFor_c ""];

)
