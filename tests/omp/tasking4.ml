open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.task ~clause:[Shared ["i"]] [sInstr "i ="];
  !! Omp.task ~clause:[Shared ["j"]] [sInstr "j ="];
  !! Omp.taskwait [cReturn_tg ~res:[cVar "i"] ()];
)
