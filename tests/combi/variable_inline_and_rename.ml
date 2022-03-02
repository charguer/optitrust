open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable.inline_and_rename [cTopFunDef "test_const_const"; cVarDef "y"];
  !! Variable.inline_and_rename [cTopFunDef "test_nonconst_const"; cVarDef "y"];
  !! Variable.inline_and_rename [cTopFunDef "test_const_nonconst"; cVarDef "y"];
  !! Variable.inline_and_rename [cTopFunDef "test_nonconst_nonconst"; cVarDef "y"];

  !! Variable.to_const [cTopFunDef "test_nonconstvect_nonconstvect"; cVarDef "b"];
  !! Variable.inline_and_rename [cTopFunDef "test_nonconstvect_nonconstvect"; cVarDef "b"];
)
