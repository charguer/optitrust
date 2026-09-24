open Optitrust
open Prelude

let _ = Flags.typechecking_mode := Flags.ProofPreserving
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_all

let int = trm_int

let _ = Run.script_cpp (fun () ->
  !! Loop.tile (int 32) ~index:"bi" ~bound:TileDivides [cFor "i"];
  !! Variable.local_name ~var:"s" ~local_var:"t" [tSpanSeq [cForBody "bi"]];

  let factor = trm_get (trm_find_var "s" []) in
  !! Accesses.shift_var ~simpl:Arith.gather_rec ~inv:true ~factor [cFor "bi"; cVarDef "t"];

  !! Loop.hoist [cVarDef "t"];
  !! Loop.fission [tBefore; cFor "bi"; cWriteVar "s"];
  !! Loop.parallel [cFor "bi" ~body:[cFor "i"]];
  !! Cleanup.std();
)
