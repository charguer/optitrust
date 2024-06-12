open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  (* replace multiple occurrences of variable "a" with variable "b" *)
  !! Expr_basic.replace (var "i") [nbMulti; cVar "a"];
  (* replace the definition of "x" with a definition of "y" --somewhat arbitrary *)
  !! Expr_basic.replace (stmt "int y = 5;") [cVarDef "x"];
  (* replace the definition of "c" with a call to "f" --somewhat arbitrary *)
  !! Expr_basic.replace (stmt "f(5);") [cVarDef "c"];
  (* replace, in the definition of "d", the first argument to f2, with the constant 3 *)
  !! Expr_basic.replace (lit "3") [cVarDef "d"; cFun "f2"; dArg 0];
  (* replace, in the definition of "e", all constants "2" with the constant "1" *)
  !! Expr_basic.replace (lit "1") [nbMulti; cVarDef "e"; cFun "f2"; cInt 2 ];
  (* replace the function "f2" with another one *)
  (* FIXME: #var-id, need to eliminate (parse) Trm_arbitrary.
  !! Expr_basic.replace (stmt "int f2(int a, int b, int c) { return a ; }") [cFunDef "f2" ];
  *)
  !!! (); (* TODO: Find how to eliminate this reparse *)
)
