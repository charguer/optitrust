open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [] [tAfter; cVarDef "a"];
  !! Omp.flush ["a"] [tFirst; tIndex ~nb:4 0; cIf (); dThen];
  !! Omp.barrier [tFirst; tIndex ~nb:4 1; cIf (); dThen];
  !! Omp.taskwait [tFirst; tIndex ~nb:4 2; cIf (); dThen];
  !! Omp.taskyield [tFirst; tIndex ~nb:4 3; cIf (); dThen];
)
