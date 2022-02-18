open Optitrust
open Target


let _ = Run.script_cpp ( fun _ ->
  
  !! Loop.fission ~split_between:true [cFor "i"];
)
