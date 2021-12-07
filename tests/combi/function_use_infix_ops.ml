open Optitrust
open Target
open Ast


let _ = Run.script_cpp (fun _ ->

    !! Function.use_infix_ops [cFunDef "main"; dFunBody];
)