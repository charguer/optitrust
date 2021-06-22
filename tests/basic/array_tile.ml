open Optitrust
open Target
(* Doesn't work *)
let _ = Run.script_cpp(fun _ ->
  
  (* show [cTypDef "T"]; *)
  !!Arrays.tile  "X"  "B" "T" [cTypDef "T"];
  Arrays.tile  "Y" "B" "U" [cTypDef "U"];  
  )
