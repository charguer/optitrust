open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 3 [cFor "i"];
  !! Omp.parallel[cSeq ~args_pred:(target_list_one_st [cFor "i"])() ];
  !! Omp.for_ ~clause:[Schedule (Static,""); Nowait] [cFor "i"];
  !! Omp.for_ ~clause:[Schedule (Static,""); Nowait] [cFor "j"];
  !! Omp.for_ ~clause:[Schedule (Static,""); Nowait] [cFor "k"];

)
