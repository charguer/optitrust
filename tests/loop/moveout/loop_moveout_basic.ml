open Optitrust
open Target

(* Note: currently, there is no check that the transformation is legitimate. E.g.:
      !! Loop_basic.move_out [cVarDef "s"]; *)

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.move_out [cVarDef "x"];
  !! Loop_basic.move_out [cVarDef "x"];
  !! Loop_basic.move_out [cVarDef "s"];
  
)
