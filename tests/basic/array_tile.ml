open Optitrust
open Target

let _ = Run.script_cpp(fun _ ->
  
  !! Arrays.tile  "X"  "B"  [cTypDef "T"];
  !! Arrays.tile  "Y" "B" [cTypDef "U"];  
)
