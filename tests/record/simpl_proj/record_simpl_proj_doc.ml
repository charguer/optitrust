open Optitrust
open Target

let _ = Run.script_cpp (*DEPRECATED ~parser:CParsers.menhir*) (fun _ ->

  !! Record_basic.simpl_proj [cVarDefs ["a"; "b"]];

)
