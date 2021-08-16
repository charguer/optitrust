open Optitrust
open Target

(* TODO: Fix me when structs and unions are added *)
let _ = Run.script_cpp (fun _ -> 
  show [cFun "compute_tree"];
)