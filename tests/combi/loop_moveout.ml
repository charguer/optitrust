open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.move_out ~upto:"i" [cVarDef "x"];
  
  !! Trace.alternative (fun () ->
    !! Loop.move_out [cVarDef "x"];
    !! Loop.move_out [cVarDef "x"];    
    !!());
  
  !! Loop.move_out [cVarDef "s"];
  !! Loop.move_out [cVarDef "s"];
)

