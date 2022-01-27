open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Loop.hoist [cFor "i"; cVarDef "x"]; (* TODO: does not work *)
  !! Loop.hoist [cFor "j"; cVarDef "y"];
  !! Loop.hoist [cFor "k"; cVarDef "x"];
)
