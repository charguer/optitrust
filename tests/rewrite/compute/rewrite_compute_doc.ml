open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Rewrite_basic.compute [cPrimCall (Prim_binop Binop_add)];

)
