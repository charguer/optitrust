open Optitrust
open Target 
open Ast
open Typ

let _ = Run.script_cpp (fun () -> 
  !! Apac.unfold_function_calls [
    nbAny; cDiff [[cFunBody "h"; cFun ""]] [[cHasTypeAst (typ_unit ())]]
  ];
)
