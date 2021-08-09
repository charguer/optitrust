open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  show [cDoWhile ()];
  !! Omp.for_ [Private ["i"]] [tIndex ~nb:2 0; cDoWhile (); cFor_c "i"];
)