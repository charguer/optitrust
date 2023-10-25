open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Record_basic.reveal_field "pos" [cTypDef "obj"];

)
