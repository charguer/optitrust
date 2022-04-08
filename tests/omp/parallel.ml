open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->
  show [occIndex 1; tBefore; cFor "a"];
  show [nbMulti; tBefore; cFor "a"];

  !! Omp.parallel_for ~collapse:2 [nbMulti; tBefore; cFor "a"];

  (* TODO:  !! Omp.(parallel_for ~collapse:2 [nbMulti; cFor "a"];*)

  (* TODO: !! Omp.parallel_collapse 2 [nbMulti; cFor "a"]; *)

  (* LATER: Collapse should be in omp.ml *)

  (*LATER !! Omp.parallel [Default (Shared_m); Private ["iam"; "nt"; "ipoints"; "istart"]] [tAfter; cVarDef "iam"];*)

)