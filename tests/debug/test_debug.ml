open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
    !! Function.use_infix_ops ~indepth:true [];

  !! Loop.shift (StartAtZero) [nbMulti; cFor "i"];
  !!! Arith_basic.(simpls_rec [expand_rec; euclidian; gather_rec]) [nbMulti; cAccesses()];

  let scale_range_for_cn (cn:int) =
    Loop_basic.scale_range ~factor:(trm_int cn) [nbMulti; cIf ~cond:[sExpr ("cn == " ^ string_of_int cn)] (); dThen; cFor "i"] in
  !! List.iter scale_range_for_cn [3; 4];

  (* repase needed before simplifications *)
  !! Arith_basic.(simpls_rec [expand_rec; gather_rec; compute]) [nbMulti; cAccesses()];

  !!! Arith_basic.simplify [nbMulti; cAccesses()];

)
