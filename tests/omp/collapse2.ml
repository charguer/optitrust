open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cFor "k"];
  !! Omp.parallel [] [tAfter;cVarDef "jlast"];
  !! Omp.for_ [Collapse 2; LastPrivate ["jlast"; "klast"]] [tBefore;cFor "k"];
  !! Omp.single [] [tBefore;cFun "printf"];
)