open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ -> 

    !! Function_basic.use_infix_ops [nbMulti;cWrite ~rhs:[cPrimPredFun is_infix_prim_fun] ()];
)