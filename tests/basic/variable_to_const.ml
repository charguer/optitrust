open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.to_const [cVarDef "x"];

  (* Tools.failure_expected (fun () ->
    !! Variable_basic.to_const [cVarDef "y"];
    (* fails at reparse of [int& u = y] with const [y] *)
    (* LATER: have a function that attempts reparse and raise an exception if reparse fails *)
    ); *)

  !! Variable_basic.to_const [cVarDef "u"];
  !! Variable_basic.to_const [cVarDef "y"];

  !! Variable_basic.to_const [cVarDef "z"];
)
