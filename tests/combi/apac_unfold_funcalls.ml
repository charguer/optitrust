open Optitrust
open Target
open Syntax

let _ = Run.script_cpp (fun () ->
  !! Apac.unfold_funcalls [
    nbAny; cDiff [[cFunBody "h"; cFun ""]] [[cHasTypeAst (typ_unit ())]]
  ];
)