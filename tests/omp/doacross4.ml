open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.for_ [Collapse 2; Ordered_c 2] [tBefore; cFor_c "i"];
  !! Omp.ordered [Depend Source] [tBefore; sInstr "B[i][j] ="];
  !! Omp.ordered [Depend (Sink ["i-1";"j"]);Depend (Sink ["i";"j-1"])] [tBefore; sInstr "C[i][j] ="];
)
