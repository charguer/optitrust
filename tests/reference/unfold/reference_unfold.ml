open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->

  (* unfold (without deletion of the definition) *)
  !! Variable_basic.unfold ~at:[cVarDef "r1"] [cVarDef "y"];
  !! Variable_basic.unfold ~at:[cVarDef "r3"] [cVarDef "b"];
  (* inline (with definition of the definition) *)
  !! Variable_basic.unfold [cVarDef "a"];

)
