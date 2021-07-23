open Optitrust
open Target


let _ = Run.script_cpp (  fun _->
  !! Variable_basic.fold [cVarDef "y"];
  !! Variable_basic.fold [cVarDef "a"];
  !! Variable_basic.fold [cVarDef "b"];
  !! Variable_basic.fold [cVarDef "v"];

)

(* TODO: ~at *)