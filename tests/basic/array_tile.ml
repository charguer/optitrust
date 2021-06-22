open Optitrust
open Target
(* Works *)
let _ = Run.script_cpp(fun _ ->
  
  Arrays.tile  "X"  "B" "T" [cTypDef "T"];
  Arrays.tile  "Y" "B" "U" [cTypDef "U"];  
  )
