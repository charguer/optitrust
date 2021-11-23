open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  !! Variable.elim_redundant [cVarDef "b"];
  !! Variable.elim_redundant ~source:[cVarDef "coef_x1"] [cVarDef "coef_x2"];
)