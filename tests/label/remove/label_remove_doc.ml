open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Label_basic.remove [cLabel "mylabel"]

)
