
open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Loop.reorder ~order:["c";"b";"a"] [cFor "a"];

)
