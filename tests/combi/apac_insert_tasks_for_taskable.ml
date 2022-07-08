open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  let tsk = Apac_basic.identify_taskable_functions [] in
  !! Apac.insert_tasks_for_taskable tsk  [nbAny; cFun "g"];

)