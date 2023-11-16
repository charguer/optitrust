open Optitrust
open Target


let _ = Run.script_cpp ( fun _ ->
  !! Loop.fission [cForBody ~body:[cVarDef "d"] "i"; tBetweenAll];
  !! Loop.fission ~nest_of:2 [nbMulti; cForBody ~body:[cVarDef "x"] "j"; tBetweenAll];
  !! Loop.fission ~nest_of:3 [nbMulti; tAfter; cFor ~body:[cVarDef "y"] "i"; cVarDef "b"];
)
