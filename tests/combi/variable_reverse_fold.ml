open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  !! Variable.reverse_fold [cVarDef "y"];

)