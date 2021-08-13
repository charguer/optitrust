open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Sequence_basic.intro 2 [cFunDef "nowait_example1";cFor "i"];
  !! Omp.parallel [] [tBefore; cFunDef "nowait_example1";cSeq ~args_pred:(target_list_one_st (cFor "i"))() ];
  !! Omp.for_ [Nowait] [tBefore; cFunDef "nowait_example1"; cFor "i"];
  !! Omp.for_ [Nowait] [tBefore; cFunDef "nowait_example1"; cFor "j"];
  !! Sequence_basic.intro 3 [cFunDef "nowait_example2";cFor "i"];
  !! Omp.parallel [] [tBefore; cFunDef "nowait_example2";cSeq ~args_pred:(target_list_one_st (cFor "i"))() ];
  !! Omp.for_ [Schedule (Static,"0"); Nowait] [tBefore; cFunDef "nowait_example2"; cFor "i"];
  !! Omp.for_ [Schedule (Static,"0"); Nowait] [tBefore; cFunDef "nowait_example2"; cFor "j"];
  !! Omp.for_ [Schedule (Static,"0"); Nowait] [tBefore; cFunDef "nowait_example2"; cFor "k"];
)