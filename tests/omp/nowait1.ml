open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  
  !! Sequence_basic.intro 2 [cFor "i"];
  !! Omp.parallel [cSeq ~args_pred:(target_list_one_st [cFor "i"])() ];
  !! Omp.for_ ~clause:[Nowait] [cFor "i"];
  !! Omp.for_ ~clause:[Nowait] [cFor "j"];

)