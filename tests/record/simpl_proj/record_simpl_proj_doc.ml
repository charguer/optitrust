open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Record_basic.simpl_proj [cVarDefs ["a"; "b"]];

)
