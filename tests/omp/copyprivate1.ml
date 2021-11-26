open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.threadprivate ["x";"y"] [tAfter; cVarDef "x"];
  !! Omp.parallel [CopyPrivate ["a";"b";"x";"y"]] [tFirst; cFunDef "init"; dFunBody];
)