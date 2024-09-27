open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [occIndex ~nb:2 0;cCall "work"];
  !! Sequence_basic.intro 1 [cSeq ~args_pred:(Target.target_list_one_st [cCall "work"]) ()];
  !! Omp.parallel [] [tFirst; cFunDef "wrong5"; dBody];
  !! Omp.critical "" [tBefore; cSeq ~args_pred:(Target.target_list_one_st [cCall "work"]) ()];
  !! Omp.barrier [tAfter; occIndex ~nb:2 0; cCall "work"];
)
