open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Generic_basic.replace "b" [nbMulti; cVar "a"];
  !! Generic_basic.replace "f1" [cFun "f" ; cVar "f"];
  !! Generic_basic.replace "int y = 5;" [cVarDef "x"];
  !! Generic_basic.replace "f(5)" [cVarDef "c"; cFun "f1"];

)
(* TODO: try
      !! Generic.replace "z" [nbMulti; cFunDef "f"; cArg "x"];
      ==> rename an argument and all of its occurences
*)



