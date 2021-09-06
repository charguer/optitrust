open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.invariant [cVarDef "x"];
  !! Loop_basic.invariant [cVarDef "x"];
  !! Loop_basic.invariant [cVarDef "s"];
  (* Note: currently, there is no check that the transformation is legitimate. E.g.:
      !! Loop_basic.invariant [cVarDef "s"]; *)
)
