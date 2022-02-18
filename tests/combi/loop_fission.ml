open Optitrust
open Target


let _ = Run.script_cpp ( fun _ ->
  
  
  show [cFor "i"];
  !! Loop.fission ~split_between:true [cFor "i"];
)
