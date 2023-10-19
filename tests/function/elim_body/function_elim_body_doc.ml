open Optitrust
open Target


let _ = Run.script_cpp (fun () ->

  !! Function.elim_body ~vars:(AddSuffix "_1") [cLabel "body"];

)
