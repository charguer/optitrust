open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.insert "while ( !omp_test_lock(lock) ) {
          }" [tAfter; cFun "something_useful"];
  !! Omp.taskyield [tFirst;cWhile (); dBody];
  !! Omp.unset_lock "lock" [tAfter; cFun "something_critical"];
)
