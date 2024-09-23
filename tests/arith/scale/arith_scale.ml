open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
  let scale_with_index_in ?inv ?pre_cast ?post_cast i tg = Arith_basic.scale ?inv ?pre_cast ?post_cast (trm_find_var i tg) tg in

  !! scale_with_index_in ~inv:true "i"  [cCellWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];
  !! scale_with_index_in "i" [cCellRead ~base:[cVar "t"] ~index:[cVar "i"] ()];
  !! scale_with_index_in "i" ~pre_cast:typ_f64 [cCellRead ~base:[cVar "u"] ~index:[cVar "i"] ()];
  !! scale_with_index_in "i" ~post_cast:typ_f32 [cCellWrite ~base:[cVar "u"] ~index:[cVar "i"] ()];

)
