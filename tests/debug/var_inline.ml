open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  !! Variable.inline [cVarDef "p2"];
  (* !! Variable.inline [cVarDef "p"]; *)
)
