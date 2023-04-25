open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel_for [cFor_c ""];
  !! Omp.target ~clause:[Map_c (To,["v1"; "v2"; "p"]); Map_c (From, ["p"])] [cFor_c ""];

)
