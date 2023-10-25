open Optitrust
open Prelude
open Target

let _ = Run.script_cpp(fun _ ->
  !! Loop.extend_range ~start:ExtendToZero [cFor "i"];
  !! Loop.extend_range ~stop:(ExtendBy (trm_int 5)) [cFor "j"];
  !! Loop.extend_range ~start:(ExtendBy (expr "ld")) ~stop:(ExtendTo (expr "u")) [cFor "k"];
  !! Loop.extend_range ~start:(ExtendTo (trm_int 2)) ~stop:(ExtendTo (trm_int 4)) [cFor "l"];
  (* TODO LATER: combi Loop.shrink_range :
     Loop_basic.split_range + ... + Loop_basic.delete_void *)
)
