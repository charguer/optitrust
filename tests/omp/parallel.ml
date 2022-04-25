open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel_for ~collapse:2 [nbMulti; tBefore; cFor "a"];

  (*LATER !! Omp.parallel [Default (Shared_m); Private ["iam"; "nt"; "ipoints"; "istart"]] [tAfter; cVarDef "iam"];*)

)