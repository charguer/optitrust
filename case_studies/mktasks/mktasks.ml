open Optitrust
open Target
open Ast




let make_malloc =
  Target.transfo_on_targets (Ast.trm_annot_remove Stackvar)


let _ = Run.script_cpp (fun () ->

  show [cVarDef "x"];

  !! make_malloc [cVarDef "y"];

  !! Variable_basic.bind "u" [nbMulti; cTopFunDef "f"; cFun "g"; dArg 1];

)



(* NOTE:
let make_malloc tg =
  Target.transfo_on_targets (fun t Ast.trm_annot_remove Stackvar t) tg
*)