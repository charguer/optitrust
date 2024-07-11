open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
    !! Function.use_infix_ops ~indepth:true [];
!! Arith_basic.(simpls_rec [expand; ]) [nbMulti; cAccesses()];
  !! Loop.shift (StartAtZero) [nbMulti; cFor "i"];
  !!! Arith_basic.(simpls_rec [expand; euclidian; gather_rec]) [nbMulti; cAccesses()];

  !! Loop_basic.scale_range ~factor:(trm_int 3) [nbMulti; cIf ~cond:[sExpr "cn == 3"] (); dThen; cFor "i"];

  (* repase needed before simplifications *)
  !! Arith_basic.(simpls_rec [expand; gather_rec]) [nbMulti; cAccesses()];

  !!! Arith_basic.simplify [nbMulti; cAccesses()];

)
