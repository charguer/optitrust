open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Function_basic.elim_infix_ops_at ~allow_identity:false [nbMulti; cFunBody "g"; cPrimPredCall Function.is_compound_assignement];

  !! Function.elim_infix_ops ~indepth:true [cFunBody "h"];
)
