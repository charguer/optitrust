open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
            !! Apac_taskify.parallel_task_group [cFunDef "g"];
            !! Apac_taskify.parallel_task_group [cFunDef "f"];
            !! Apac_taskify.parallel_task_group [cFunDef "h"];
            !! Apac_taskify.parallel_task_group [cFunDef "main"];

)
