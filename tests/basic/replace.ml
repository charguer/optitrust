open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* replace multiple occurrences of variable "a" with variable "b" *)
  !! Generic_basic.replace "b" [nbMulti; cVar "a"];
  (* replace the function call to "f" with a function call to "f1" *)
  !! Generic_basic.replace "f1" [cVar "f"];
  (* replace the definition of "x" with a definition of "y" --somewhat arbitrary *)
  !! Generic_basic.replace "int y = 5;" [cVarDef "x"];
  (* replace the definition of "c" with a call to "f" --somewhat arbitrary *)
  !! Generic_basic.replace "f(5);" [cVarDef "c"];
  (* replace, in the definition of "d", the first argument to f2, with the constant 3 *)
  !! Generic_basic.replace "3" [cVarDef "d"; cFun "f2"; cArg 0];
  (* replace, in the definition of "e", all constants "2" with the constant "1" *)
  !! Generic_basic.replace "1" [nbMulti; cVarDef "e"; cFun "f2"; cInt 2 ];
  (* replace the function "f2" with another one *)
  !! Generic_basic.replace "int f2(int a, int b, int c) { return a ; }" [cFunDef "f2" ];
)
