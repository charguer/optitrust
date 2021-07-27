open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! Variable_basic.inline ~at:[cVarDef "b"] [cVarDef "a"];
   !! Variable_basic.inline ~delete:true [cVarDef "c"];
   !! Variable_basic.inline ~at:[cVarDef "y"] [cVarDef "x"];
   !! Variable_basic.inline ~delete:true [cVarDef "z"];
)
