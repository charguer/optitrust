open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [Num_threads 3; Shared ["x"]] [tAfter; sInstr "x = 2"];
  !! Omp.barrier [tBefore; tIndex ~nb:2 1;cIf ()];
)
