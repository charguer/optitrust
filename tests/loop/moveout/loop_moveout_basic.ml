open Optitrust
open Target

(* Note: currently, there is no check that the transformation is legitimate. E.g.:
      !! Loop_basic.move_out [cVarDef "s"]; *)

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.move_out [cFunBody "test"; cVarDef "x"];
  !! Loop_basic.move_out [cFunBody "test"; cVarDef "x"];
  !! Loop_basic.move_out [cFunBody "test"; cVarDef "s"];

  !! Loop_basic.move_out [cFunBody "var"; sInstr "x = 3"];

  !! Loop_basic.move_out [cFunBody "arr"; cFor ~body:[cArrayWrite "x"] "j"];
)
