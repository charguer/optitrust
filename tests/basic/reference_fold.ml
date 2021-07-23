open Optitrust
open Target


let _ = Run.script_cpp (  fun _->
  !! Variable.fold [cVarDef "y"];
  !! Variable.fold [cVarDef "a"];
  !! Variable.fold [cVarDef "b"];
  !! Variable.fold [cVarDef "v"];

)

(* TODO: ~at *)