open Optitrust
open Target
(* Doesn't work *)
let _ = Run.script_cpp(fun _ ->
  Arrays.tile  "X"  "B" "T" [cTypDef "T"];
  Arrays.tile  "Y" "B" "U" [cTypDef "T"];  
  )
