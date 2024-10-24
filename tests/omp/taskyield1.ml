open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.insert ~reparse:true (stmt "while ( !omp_test_lock(lock) ) {\n
          printf(\"Do something\");}") [tAfter; cCall "something_useful"];
  !! Omp.taskyield [cCall "printf"];
  !! Omp.unset_lock "lock" [tAfter; cCall "something_critical"];
)
