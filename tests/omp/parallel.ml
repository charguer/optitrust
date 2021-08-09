open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ -> 
  !! Omp.parallel [Default_c (Shared); Private ["iam"; "nt"; "ipoints"; "istart"]] [tAfter; cVarDef "iam"];
  
)