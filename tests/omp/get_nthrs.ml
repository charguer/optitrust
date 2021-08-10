open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.get_thread_num "i" [tBefore; cFun "work"];
  !! Omp.parallel [Private ["i"]] [tAfter;cVarDef "i"];
)
