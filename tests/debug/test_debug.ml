open Optitrust
open Target


let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->
  
  !! Variable_basic.to_const [cVarDef "v"];

  !! Variable_basic.to_const [cVarDef "w"];

  !! Variable_basic.to_const [cVarDef "p"];

  !! Variable_basic.to_const [cVarDef "q"];
  
  
  !! Variable_basic.to_const [cVarDef "y"];
  !! Variable_basic.to_const [cVarDef "z"];

  !! Variable_basic.to_const [cVarDef "v"];
  !! Variable_basic.to_const [cVarDef "w"];

  !! Variable_basic.to_const [cVarDef "p"];
  !! Variable_basic.to_const [cVarDef "q"];
  !! Variable_basic.to_const [cVarDef "r"];
)
