open Optitrust
open Prelude


let _ = Run.script_cpp (fun () ->

  !! Matrix_basic.insert_alloc_dim (expr "N2") [cVarInit "p"];
  !! Matrix_basic.insert_alloc_dim (expr "N2") [cVarInit "q"];

  !!! (); (* TODO: Find how to eliminate this reparse *)

)
