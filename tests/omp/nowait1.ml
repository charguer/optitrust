open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cFor_c "i"];
  !! Omp.parallel [] [tBefore; cSeq ~args_pred:(target_list_one_st (cFor_c "i"))() ];
  !! Omp.for_ [Nowait] [tBefore; cFor_c "i"];
  !! Omp.for_ [Nowait] [tBefore; cFor_c "j"];
)