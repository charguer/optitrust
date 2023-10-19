open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.insert_alloc_dim (expr "N2") [cMalloc ()];

)
