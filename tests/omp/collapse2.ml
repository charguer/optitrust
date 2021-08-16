open Optitrust
open Target

(* TODO: Fix the issue with indices for simple loops *)
let _ = Run.script_cpp (fun _ ->
  
  !! Sequence_basic.intro 2 [cFor "k"];
  !! Omp.parallel [] [tAfter;cVarDef "jlast"];
  !! Omp.for_ [Collapse 2; LastPrivate ["jlast"; "klast"]] [tBefore;cFor "k"];
  !! Omp.single [] [tBefore;cFun "printf"];
  (*!! Omp.parallel [NumThreads 2] [tAfter; cFunDef "sub2"; cVarDef "j"];
  !! Omp.for_ [Collapse 2; Ordered_c 0; Private ["j"; "k"] ; Schedule (Static, "3")] [tBefore; cFunDef "sub2"; cFor "k"]; *)
  (* !! Omp.ordered [] [tBefore; cFunDef "sub2"; cFun "printf"]; *) (* TODO: Fix the issue when matching printf call *)
)