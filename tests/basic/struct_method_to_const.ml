open Optitrust
open Target 


let _ = Run.script_cpp (fun _ ->

  ();
  (* show [cTypDef "Method_const"]; *)
  !! Struct_basic.method_to_const "get_x" [cTypDef "Method_const"]

)
