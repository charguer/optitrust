open Optitrust
open Prelude


let _ = Run.script_cpp (fun () ->

   !! Arith_basic.shift ~inv:true (var "i")  [cCellWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];
   !! Arith_basic.shift  (var "i") [cCellRead ~base:[cVar "t"] ~index:[cVar "i"] ()];
   !! Arith_basic.shift (var "i") ~pre_cast:(typ_double ()) [cCellRead ~base:[cVar "u"] ~index:[cVar "i"] ()];
   !! Arith_basic.shift (var "i") ~post_cast:(typ_float ()) [cCellWrite ~base:[cVar "u"] ~index:[cVar "i"] ()];

)
