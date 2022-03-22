open Optitrust
open Target

let _ = Run.script_cpp (fun () -> 

  !! Matrix_basic.malloc_to_malloc_aligned (lit "64") [cFun "MMALLOC1"];
)