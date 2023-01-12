open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  
  !! Sequence_basic.intro ~mark:"tskg" 2 [cFun "long_running_task"];
  
  !! Omp.taskgroup [cMark "tskg"];
  !! Omp.taskloop ~clause:[Private ["j"]; Grainsize 500; Nogroup] [occIndex 1; cFor_c ""];
  !! Omp.task [cFun "long_running_task"];
  !! Marks.remove "tskg" [cMark "tskg"];
  

)