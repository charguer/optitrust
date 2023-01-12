open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Sequence_basic.intro 1 [cFor_c ""];
     let tg_seq = cSeq ~args:[[cFor_c ""]] () in 
  !! Sequence_basic.intro 1 [tg_seq];
  !! Omp.single [tg_seq];
  !! Omp_basic.parallel [cSeq ~args:[[tg_seq]] ()];
  !! Omp.task [cFun "process"];

)
