open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Typedef_basic.insert_copy "vect2" [cTypDef "vect"];

)
