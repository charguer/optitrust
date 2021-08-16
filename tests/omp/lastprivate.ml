open Optitrust
open Target


(* This should work fine after fixing the issue with cFor without declaration *)
let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [] [tFirst; cFunDef "lastpriv"; dBody];
  !! Omp.parallel_for [LastPrivate ["a"]] [tBefore; cFor "i"];
)
