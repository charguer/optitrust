open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->

  !! Arith_basic.scale ~inv:true (var "i")  [cCellWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];
  !! Arith_basic.scale  (var "i") [cCellRead ~base:[cVar "t"] ~index:[cVar "i"] ()];
  !! Arith_basic.scale (var "i") ~pre_cast:typ_f64 [cCellRead ~base:[cVar "u"] ~index:[cVar "i"] ()];
  !! Arith_basic.scale (var "i") ~post_cast:typ_f32 [cCellWrite ~base:[cVar "u"] ~index:[cVar "i"] ()];

)
