open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.disable_stringreprs := true

let _ = Run.script_cpp (fun () ->

  !! Arith_basic.(simpls_rec [expand; gather_rec; compute]) [nbMulti; cAccesses()];

(*
  !! Function.use_infix_ops ~indepth:true [];
  !! Arith_basic.(simpls_rec [expand_rec; euclidian; gather_rec]) [nbMulti; cAccesses()];
  !! Loop.shift (StartAtZero) [nbMulti; cFor "i"];
  let scale_range_for_cn (cn:int) =
    Loop_basic.scale_range ~factor:(trm_int cn) [nbMulti; cIf ~cond:[sExpr ("cn == " ^ string_of_int cn)] (); dThen; cFor "i"] in
  !! List.iter scale_range_for_cn [3; 4];
  !! Loop_basic.scale_range ~factor:(var "cn") [nbMulti; cIf ~cond:[sExpr "cn == 4"] (); dElse; cFor "i"];
  (* repase needed before simplifications *)
  !! Arith_basic.(simpls_rec [expand_rec; gather_rec; compute]) [nbMulti; cAccesses()];
  !!! Arith_basic.simplify [nbMulti; cAccesses()];
*)
)
