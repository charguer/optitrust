open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cFor "k"];
  !! Omp.parallel [cVarDef "jlast"];
  !! Omp.for_ ~clause:[Collapse 2; LastPrivate ["jlast"; "klast"]] [cFor "k"];
  !! Omp.single [cFun "printf"];

)