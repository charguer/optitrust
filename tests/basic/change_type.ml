open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 
  !! Typedef_basic.copy "vect2" [cTypDef "vect"];
  !! Generic_basic.change_type "vect2" [cVarDef "w"];

)