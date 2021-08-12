open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.threadprivate ["work";"size";"toi"] [tBefore; cFunDef "build"];
  !! Omp.parallel [Copyin ["toi";"size"]] [tAfter; sInstr "size ="];
)