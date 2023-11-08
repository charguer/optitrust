open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.move_out [cFunBody "var"; sInstr "x = 3"];
  !! Trace.failure_expected (fun () ->
    Loop_basic.move_out [cFunBody "var_wrong1"; sInstr "x = i"]);
  !! Trace.failure_expected (fun () ->
    Loop_basic.move_out [cFunBody "var_wrong2"; sInstr "x = 3"]);
  !! Trace.failure_expected (fun () ->
    Loop_basic.move_out [cFunBody "var_wrong3"; sInstr "x += 3"]);
  !! Trace.failure_expected (fun () ->
    Loop_basic.move_out [cFunBody "var_wrong4"; sInstr "x = 3"]);

  !! Loop_basic.move_out [cFunBody "arr"; cFor ~body:[cArrayWrite "x"] "j"];
  !! Trace.failure_expected (fun () ->
    Loop_basic.move_out [cFunBody "arr_wrong1"; cFor ~body:[cArrayWrite "x"] "j"]);

  !! Loop_basic.move_out [cFunBody "test"; cVarDef "x"];
  !! Loop_basic.move_out [cFunBody "test"; cVarDef "x"];
  !! Loop_basic.move_out [cFunBody "test"; cVarDef "s"];
  !! Trace.failure_expected (fun () ->
    Loop_basic.move_out [cFunBody "test"; cVarDef "s"]);
)
