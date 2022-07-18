open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->


  !! Omp.task [nbMulti; cFun "postorder_traverse"];
  !! Omp.taskwait [cFun "process"];

)
