open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Cast_basic.insert Typ.(typ_double ()) [cReadVar "a"];

)
