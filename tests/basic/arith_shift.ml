open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->
   
   !! Arith_basic.shift ~neg:true (code "i")  [cSet ~lhs:[cIndexGet ~base:[cVar "t"] [cVar "i"]] ()];

   show [cIndexGet ~base:[cVar "t"] [cVar "i"]];
   (* !! Arith_basic.shift (code "i") ~neg:true [cGet ~rhs:[cIndexGet ~base:[cVar "t"] [cVar "i"]] ()]; *)

   (* TODO: Fix me *)
   (* !! Arith_basic.shift (code "i") ~pre_cast:(typ_double ()) [nbMulti;cIndexGet ~base:[cVar "u"] [cVar "i"]]; *)

   !! Arith_basic.shift (code "i") ~neg:true ~pre_cast:(typ_float ()) [cVar "b"];
)
