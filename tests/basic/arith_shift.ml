open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->
   
   !! Arith_basic.shift (code "i") [cSet ~lhs:[sExpr "t"] ()];
   (* !! Arith_basic.shift ~neg:true (code "i") [cSet ~lhs:[cStrict; cAccess ()] ()]; *)
   (* !! Arith_basic.shift ~neg:false (code "i") [cGet ~arg:[sExpr "t"] ()]; *)
    (*
   !! Arith_basic.shift (code "i") ~pre_cast:typ_double [cGet ~arg:[sExpr "u[i]"];
   !! Arith_basic.shift (code "")~neg:true (code "i") ~post_cast:typ_float [cSet ~lhs:[sExpr "u[i]"]; *)
)
