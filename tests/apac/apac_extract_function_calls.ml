open Optitrust
open Target 
open Ast
open Typ

let _ = Run.script_cpp (fun () ->
            !! Apac_preprocessing.extract_function_calls [
                nbAny;
                cDiff [[cFunBody "h"; cFun ""]] [[cHasTypeAst (typ_unit ())]]
              ];
          );
        Apac_reset.tnt_blast ()
