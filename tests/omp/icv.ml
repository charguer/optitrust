open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [tIndex ~nb:2 0; cFun "printf"];
  !! Sequence_basic.intro 1 [tIndex ~nb:2 1; cFun "printf"];
  !! Sequence_basic.intro 1 [tIndex ~nb:2 0; cSeq ~args:[[cFun "printf"]] ()];
  !! Sequence_basic.intro 1 [tIndex ~nb:2 1; cSeq ~args:[[cFun "printf"]] ()];
  !! Sequence.intro ~start:[tFirst; cFunDef "main"; dBody] ~stop:[tBefore;cReturn] ();
  !! Omp.parallel [] [tFirst; cFunDef "main"; dBody];
  !! Omp.set_num_threads 2 [tFirst; cFunDef "main"; dBody];
  !! Omp.set_dynamic 0 [tFirst; cFunDef "main"; dBody];
  !! Omp.set_max_active_levels 8 [tFirst; cFunDef "main"; dBody];
  !! Omp.set_nested 1 [tFirst; cFunDef "main"; dBody];
  !! Omp.set_num_threads 3 [tBefore; tIndex ~nb:2 0;cSeq ~args:[[cSeq ~args:[[cFun "printf"]] ()]] ()];
  !! Omp.parallel [] [tBefore; tIndex ~nb:2 0;cSeq ~args:[[cSeq ~args:[[cFun "printf"]] ()]] ()];
  !! Omp.barrier [tBefore; tIndex ~nb:2 1;cSeq ~args:[[cSeq ~args:[[cFun "printf"]] ()]] ()];
  !! Omp.single [] [tBefore; tIndex ~nb:2 1;cSeq ~args:[[cSeq ~args:[[cFun "printf"]] ()]] ()];
  !! Omp.set_num_threads 4 [tBefore; tIndex ~nb:2 0; cSeq ~args:[[cFun "printf"]] ()];
  !! Omp.single [] [tBefore; tIndex ~nb:2 0;cSeq ~args:[[cFun "printf"]] ()];

)
