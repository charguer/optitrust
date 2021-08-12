open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.task [Shared ["i"]] [tBefore;sInstr "i ="];
  !! Omp.task [Shared ["j"]] [tBefore;sInstr "j ="];
  !! Omp.taskwait [tBefore; tIndex ~nb:2 1;cReturn];
)
