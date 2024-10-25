open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true

let _ = Run.script_cpp (fun () ->
  !! Resources.ensure_computed ();

  !! Loop.shift (StartAtZero) [nbMulti; cFor "i"];
  !! Loop.scale_range ~factor:(trm_find_var "cn" []) [nbMulti; cFor "i"];

  (* repase needed before simplifications *)
  !! Arith_basic.(simpls_rec [expand; gather_rec; compute]) [nbMulti; cAccesses()];

  !!! Arith_basic.simplify [nbMulti; cAccesses()];

)
