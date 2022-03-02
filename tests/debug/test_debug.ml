open Optitrust
open Target


let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->
  
  !! Variable.inline_and_rename [cTopFunDef "test_nonconst_nonconst"; cVarDef "b"];
)
