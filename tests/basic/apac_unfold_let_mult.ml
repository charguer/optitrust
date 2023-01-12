open Optitrust
open Target 

let _ = Run.script_cpp (fun () -> 

  !! Apac.unfold_let_mult [nbMulti; cVarsDef "" ];

)