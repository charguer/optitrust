open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! Variable_basic.inline_at [cVarDef "b"] [cVarDef "a"];
   !! Variable_basic.inline [cVarDef "c"];
   !! Variable_basic.inline_at [cVarDef "y"] [cVarDef "x"];
   !! Variable_basic.inline [cVarDef "z"];
)
