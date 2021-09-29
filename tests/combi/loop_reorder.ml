open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Loop.reorder ~order:["bx";"by";"x";"cx";"cy";"y" ] [cFor "cx"];
)
