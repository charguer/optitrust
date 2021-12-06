open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  !! Sequence.intro_targets [cVarDef ""];

)
