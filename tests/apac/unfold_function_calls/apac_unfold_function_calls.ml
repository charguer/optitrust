open Optitrust
open Target 
open Ast
open Typ

let _ = Run.script_cpp (fun () ->
            reset_fresh_var_int ();
            !! Apac.unfold_function_calls [
                nbAny;
                cDiff [[cFunBody "h"; cFun ""]] [[cHasTypeAst (typ_unit ())]]
              ];
          )
