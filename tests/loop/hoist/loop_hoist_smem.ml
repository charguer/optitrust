open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->
  !! Resources.ensure_computed ();
  !! Loop.hoist ~nest_of:2 ~inline:false [nbMulti; cFor "z_i"; cVarDefs ["z_a"]];
(*  !! Loop.hoist ~inline:false [cFor "i"; cVarDef "x"];
  !! Loop.hoist [cFor "j"; cVarDef "y"];
  (* LATER: avoid requiring tmp names, leverage #var-id. *)
  !! Loop.hoist ~tmp_names:"x2_step${i}" ~inline:false ~name:"xk" [cFor "k"; cVarDef "x"];
  !! Loop.hoist ~nest_of:2 [cFor "l"; cVarDef "x"];
  !! Loop.hoist ~nest_of:3 ~name:"xa" [cFor "a"; cVarDef "x"];
  !! Loop.hoist_alloc ~indep:["mi"] ~dest:[tBefore; cFor "mi"] [cVarDef "m"];
  !! Loop.hoist [nbMulti; cFor "v"; cVarDefs ["a"; "b"] ];*)
)
