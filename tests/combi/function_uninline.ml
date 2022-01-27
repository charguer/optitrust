open Optitrust
open Target

(* TODO: find out why the function body are hidden, even though
FLAGS := -disable-light-diff
appears in the makefile
*)

let _ = Run.script_cpp (fun _ ->

    !! Function.uninline ~fct:[cTopFunDef "f"] [cVarDef "b"];

)
