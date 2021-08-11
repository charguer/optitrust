open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.parallel [Shared_c ["x";"y"]; Private ["ix_next";"iy_next"]] [tAfter; cVarDef "ix_next"];
  !! Omp.critical "xaxis" [tBefore; sInstr "ix_next = dequeue(x)"];
  !! Omp.critical "yaxis" [tBefore; sInstr "iy_next = dequeue(y)"];
)
