open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->
   
   !! Arith_basic.shift ~neg:true (code "i")  [cCellWrite ~base:[cVar "t"] [cVar "i"]];
   !! Arith_basic.shift  (code "i") [cCellRead ~base:[cVar "t"] [cVar "i"]];
   !! Arith_basic.shift (code "i") ~pre_cast:(typ_double ()) [cCellRead ~base:[cVar "u"] [cVar "i"]];
   !! Arith_basic.shift (code "i") ~post_cast:(typ_float ()) [cCellWrite ~base:[cVar "u"] [cVar "i"]; dRHS];
)
