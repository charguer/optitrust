open Optitrust
open Target 
open Ast
open Typ

let () =
  Apac_reset.tnt_blast ();
  Run.script_cpp ~check_syntax_at_end:true (fun () ->
      !! Apac_preprocessing.unfold_function_calls [
          nbAny;
          cFunBody "h";
          cFun ""
        ];
      !! Apac_preprocessing.detach_function_calls [
          nbAny;
          cFunBody "h";
          cVarDef ""
        ];
    )
