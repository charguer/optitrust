open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  (* TODO
  !! Matrix_basic.storage_folding [cVarDef "tmp"];
  ~storage:Circular
  ~storage:Variables
*)
  ()
)

"
TODO
"

let _ = Run.script_cpp (fun _ ->
    (* TODO
  !! Matrix_basic.storage_folding ~storage:Circular [cVarDef "tmp"];
  !! Matrix_basic.storage_folding ~storage:Variables [cVarDef "tmp2"];
*)
  ()
)