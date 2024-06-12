open Optitrust
open Prelude


let _ = Run.script_cpp (fun () ->

  !! Matrix_basic.insert_alloc_dim (expr "N2") [cMalloc ()];
  !! Matrix_basic.insert_alloc_dim (expr "N2") [cCalloc ()];

  !!! (); (* TODO: Find how to eliminate this reparse *)

)
