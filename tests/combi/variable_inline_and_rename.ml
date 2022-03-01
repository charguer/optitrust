open Optitrust
open Target

(* TODO: remove ~parsers option where it works with both *)
let _ = Run.script_cpp ~parser:Parsers.Clang (fun _ ->

  !! Variable.inline_and_rename [cTopFunDef "test_const_const"; cVarDef "y"];
  !! Variable.inline_and_rename [cTopFunDef "test_nonconst_const"; cVarDef "y"];
  !! Variable.inline_and_rename [cTopFunDef "test_const_nonconst"; cVarDef "y"];
  !! Variable.inline_and_rename [cTopFunDef "test_nonconst_nonconst"; cVarDef "y"];
)
