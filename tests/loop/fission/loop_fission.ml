open Optitrust
open Target


let _ = Run.script_cpp ( fun _ ->
  !! Loop.fission_all_instrs [cFor ~body:[cVarDef "d"] "i"];
  !! Loop.fission_all_instrs ~nest_of:2 [nbMulti; cFor ~body:[cVarDef "x"] "i"];
  !! Loop.fission ~nest_of:3 [nbMulti; tAfter; cFor ~body:[cVarDef "y"] "i"; cVarDef "b"];
)
