open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  show [nbAny; sInstr ~substr:false "i++"];
  show [sInstr "j <"];
  
  (* TODO ARTHUR: report the "show" in the AST not in the diff. *)
)
