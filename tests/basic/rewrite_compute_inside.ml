open Optitrust
open Target
let _ = Run.script_cpp (fun _ -> 

  !! Rewrite_basic.compute_inside [nbMulti;cPrimPredFun (fun (Prim_binop _) -> true) ];
)