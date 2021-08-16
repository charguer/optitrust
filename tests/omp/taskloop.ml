open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.taskgroup [tFirst; cFunDef "parallel_work";dBody];
  !! Omp.task [] [tBefore; cFun "long_running_task"];
  !! Omp.taskloop [Private ["j"]; Grainsize 500; Nogroup] [tBefore; cFor "i"];
)