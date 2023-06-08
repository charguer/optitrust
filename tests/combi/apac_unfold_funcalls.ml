open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  Ast_data.fill_fun_defs_tbl (get_ast());

  (* !! Apac.unfold_funcalls [cFunDef "h"]; *)
  !! Variable.bind "d" [nbMulti; cFunBody "h"; cFun "f"];
  !! Variable.init_detach [nbMulti; cVarDef "d"];
)