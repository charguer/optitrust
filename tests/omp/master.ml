open Optitrust
open Target

(* TODO: Fix the issue with clang transforming PreInc to PostInc *)
let _ = Run.script_cpp (fun _ ->
  show [cDoWhile ()];
  !! Omp.for_ [Private ["i"]] [tBefore; tIndex ~nb:2 0; cFor "i"];
  !! Omp.single [] [tBefore; cSeq ~args:[sInstr "toobig = 0"] ()];
  !! Omp.for_ [Private ["i"; "y"; "error"]] [tBefore; tIndex ~nb:2 1; cFor "i"];
  
)