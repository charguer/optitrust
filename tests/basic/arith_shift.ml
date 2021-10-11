open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->
   
   !! Arith_basic.shift ~neg:true (code "i")  [cIndexSet ~base:[cVar "t"] [cVar "i"]];
   !! Arith_basic.shift  (code "i") [cIndexGet ~base:[cVar "t"] [cVar "i"]];
   !! Arith_basic.shift (code "i") ~pre_cast:(typ_double ()) [cIndexGet ~base:[cVar "u"] [cVar "i"]];
   !! Arith_basic.shift (code "i") ~post_cast:(typ_float ()) [cIndexSet ~base:[cVar "u"] [cVar "i"]; dRHS];
)
