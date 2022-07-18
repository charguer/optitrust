open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel_for [cFor_c ""];
  !! Omp.target ~clause:[Map_c (To, ["v1[0:N]";"v2[:N]"]);Map_c (From, ["p[0:N]"])] [cFor_c ""];

)
