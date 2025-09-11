open Optitrust
open Target


let _ = Run.script_cpp (fun () ->
  !! Loop.hoist ~inline:false [cFor "i"; cVarDef "x"];
  !! Loop.hoist [cFor "j"; cVarDef "y"];
  (* LATER: avoid requiring tmp names, leverage #var-id. *)
  !! Loop.hoist ~tmp_names:"x2_step${i}" ~inline:false ~name:"xk" [cFor "k"; cVarDef "x"];
  !! Loop.hoist ~nest_of:2 [cFor "l"; cVarDef "x"];
  !! Loop.hoist ~nest_of:3 ~name:"xa" [cFor "a"; cVarDef "x"];
  !! Loop.hoist_alloc ~indep:["mi"] ~dest:[tBefore; cFor "mi"] [cVarDef "m"];
  !! Loop.hoist [nbMulti; cFor "v"; cVarDefs ["a"; "b"] ];
  !!! ();
)
