open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.parallel [Shared ["x";"y"]; Private ["ix_next";"iy_next"]] [tAfter; cVarDef "ix_next"];
  !! Omp.critical "xaxis" ~hint:"omp_lock_hint_contented" [tBefore; sInstr "ix_next = dequeue(x)"];
  !! Omp.critical "yaxis" ~hint:"omp_lock_hint_contented" [tBefore; sInstr "iy_next = dequeue(y)"];
)
