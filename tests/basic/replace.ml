open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Generic_basic.replace "b" [nbMulti; cVar "a"];
  !! Generic_basic.replace "f1" [cFun "f" ; cVar "f"];
  !! Generic_basic.replace "int y = 5;" [cVarDef "x"];
  !! Generic_basic.replace "f(5)" [cVarDef "c"; cFun "f1"];
  !! Generic_basic.replace "3" [cVarDef "d"; cFun "f2"; cArg 0];
  !! Generic_basic.replace "1" [cVarDef "d"; cFun "f2"; cArg 2];
)

