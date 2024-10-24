open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->


  !! Omp.task [nbMulti; cCall "postorder_traverse"];
  !! Omp.taskwait [cCall "process"];

)
