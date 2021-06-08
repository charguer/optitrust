open Optitrust
open Run

(* Doesn't work *)
let _ = run_unit_test(fun _ ->
  Arrays.tile  "X"  "B" "T" [cTypDef "T"];
  Arrays.tile  "Y" "B" "U" [cTypDef "T"];  
  )

