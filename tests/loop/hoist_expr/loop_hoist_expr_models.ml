open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->
  !! Loop.hoist_expr ~dest:[tBefore; cFor "i"] "t2" [cFor "i"; cArrayRead "t"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "l"] "t02" ~indep:["l"; "m"] [cFor "l"; cArrayRead "t"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "a"] "a2" ~indep:["b"; "c"] [cVarDef "x"; cVar "a"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor ~body:[cVarDef "x"] "a"] "u2" ~indep:["b"; "c"] [cVarDef "x"; cArrayRead "u"];
  !!! ();
)
