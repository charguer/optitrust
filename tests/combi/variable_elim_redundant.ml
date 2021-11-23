open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  !! Variable.elim_redundant [cVarDef "b"];
  !! Variable.elim_redundant [cVarDef "coef_x2"];
)