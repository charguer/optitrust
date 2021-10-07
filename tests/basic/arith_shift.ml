open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->
   
   (* !! Arith_basic.shift ~neg:true (code "i")  [cIndexSet ~base:[cVar "t"] [cVar "i"]]; *)
   show [tIndex ~nb:2 1; cIndexGet ~base:[cVar "t"] [cVar "i"]];
   !! Arith_basic.shift  (code "i") [tIndex ~nb:2 1; cIndexGet ~base:[cVar "t"] [cVar "i"]];
   (* show [cVarDef "a"; cIndexGet [cVar "i"]]; *)
   (* !! Arith_basic.shift (code "i") ~pre_cast:(typ_double ()) [cVarDef "a"; cIndexGet [cVar "i"]]; *)
   (* !! Arith_basic.shift (code "i") ~post_cast:(typ_float ()) [cVar "b"]; *)
)
