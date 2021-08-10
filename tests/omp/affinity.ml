open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Sequence_basic.intro 1 [cFunDef "main"; cFun "work"];
  !! Omp.parallel [Proc_bind Spread; Num_threads 4] [tBefore;cFunDef "main";cSeq ~args:[cFun "work"] ()];
  !! Sequence_basic.intro 1 [cFunDef "foo"; cFun "work"];
  !! Omp.parallel [Num_threads 16; Proc_bind Spread] [tBefore;cFunDef "foo";cSeq ~args:[cFun "work"] ()];
  !! Sequence_basic.intro 1 [cFunDef "bar"; cFun "work"];
  !! Omp.parallel [Proc_bind Close; Num_threads 4] [tBefore;cFunDef "bar";cSeq ~args:[cFun "work"] ()];
  !! Sequence_basic.intro 1 [cFunDef "foo1"; cFun "work"];
  !! Omp.parallel [Num_threads 16; Proc_bind Close] [tBefore;cFunDef "foo1";cSeq ~args:[cFun "work"] ()];
  !! Sequence_basic.intro 1 [cFunDef "bar1"; cFun "work"];
  !! Omp.parallel [Proc_bind Master_pb; Num_threads 4] [tBefore;cFunDef "bar1";cSeq ~args:[cFun "work"] ()];
  
)
