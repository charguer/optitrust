open Optitrust
open Target 


let _ = Run.script_cpp (fun () -> 

  let tsk = Apac_basic.identify_taskable_functions [] in
  !! Apac_basic.bind_taskable_calls tsk [];

)
