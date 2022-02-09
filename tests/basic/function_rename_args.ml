open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->

  !! Function_basic.rename_args ["x2"; "y2"] [cFunDef "f"];
)