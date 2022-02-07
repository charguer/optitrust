open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
    !! Variable_basic.change_type "long" [cVarDef "a"];
  )
"
int a = 0;
"

let _ = Flags.dump_ast_details := true


let _ = Run.script_cpp (fun _ ->
  
  !! Variable_basic.change_type "vect2" [cVarDef "w"];

)
