open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->
  

   show [cGet ~arg:[sExpr "t[i]"] ()];
   !! Generic.data_shift (code "i") [cSet ~lhs:[sExpr "t[i]"] ()];
   (* !! Generic.data_shift ~neg:true (code "i") [cSet ~lhs:[cStrict; cAccess ()] ()]; *)
   (*!! Generic.data_shift ~neg:false (code "i") [cGet ~arg:[sExpr "t[i]"];

   !! Generic.data_shift (code "i") ~pre_cast:typ_double [cGet ~arg:[sExpr "u[i]"];
   !! Generic.data_shift (code "")~neg:true (code "i") ~post_cast:typ_float [cSet ~lhs:[sExpr "u[i]"]; *)
)
