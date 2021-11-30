open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->
   
   !! Arith_basic.shift ~neg:true (expr "i")  [cCellWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];
   !! Arith_basic.shift  (expr "i") [cCellRead ~base:[cVar "t"] ~index:[cVar "i"] ()];
   !! Arith_basic.shift (expr "i") ~pre_cast:(typ_double ()) [cCellRead ~base:[cVar "u"] ~index:[cVar "i"] ()];
   !! Arith_basic.shift (expr "i") ~post_cast:(typ_float ()) [cCellWrite ~base:[cVar "u"] ~index:[cVar "i"] ()];
)
