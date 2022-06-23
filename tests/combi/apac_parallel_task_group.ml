open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Apac.parallel_task_group [cFunDef "g"];

  !! Apac.parallel_task_group [cFunDef "f"];

  !! Apac.parallel_task_group [cFunDef "h"];

)
