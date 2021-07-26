open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.invariant [cVarDef "x"];
  !! Loop_basic.invariant [cVarDef "x"];
  !! Loop_basic.invariant [cVarDef "s"];
  !! Loop_basic.invariant [cVarDef "s"];
)
