open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [cFor "k"];
  !! Omp.parallel ~clause:[Num_threads "2"] [cVarDef "a"];
  !! Omp.for_  ~clause:[Collapse 2; Ordered_c 0;Private ["j"; "k"]; Schedule (Static, "3")] [cFor "k"];
  !! Omp.ordered [cFun "printf"];

)
