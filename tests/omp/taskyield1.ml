open Optitrust
open Target
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.insert ~reparse:true (stmt "while ( !omp_test_lock(lock) ) {\n
          printf(\"Do something\");}") [tAfter; cFun "something_useful"];
  !! Omp.taskyield [cFun "printf"];
  !! Omp.unset_lock "lock" [tAfter; cFun "something_critical"];
)
