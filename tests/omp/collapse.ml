open Optitrust
open Target

(* TODO: Fix the issue with indices for simple loops *)
let _ = Run.script_cpp (fun _ ->
  !! Omp.for_ [Collapse 2; Private ["i"; "k"; "j"]] [tBefore; cFunDef "sub1"; cFor "k"];
  !! Omp.parallel [] [tAfter; cFunDef "test"; cVarDef "j"];
  !! Omp.for_ [Collapse 2; LastPrivate ["jlast"; "klast"]] [tBefore; cFunDef "test"; cFor "k"];
  !! Omp.single [] [tAfter; cFunDef "test"; cFor "k"];
  !! Omp.parallel [NumThreads 2] [tAfter; cFunDef "sub2"; cVarDef "j"];
  !! Omp.for_ [Collapse 2; Ordered_c; Private ["j"; "k"] ; Schedule (Static, 3)] [tBefore; cFunDef "sub2"; cFor "k"];
  (* !! Omp.ordered [tBefore; cFunDef "sub2"; cFun "printf"]; *) (* TODO: Fix the issue when matching printf call *)
)