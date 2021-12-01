open Optitrust
open Target

let _  = Run.script_cpp (fun _ ->
  !! Variable_basic.rename ~into:"a1" [cVarDef "a"];
  !! Variable_basic.rename ~into:"b1" [cVarDef "b"];
)