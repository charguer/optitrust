open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

    !! Function_basic.bind_intro ~fresh_name:"a" [sExpr "f(1)"];
)
