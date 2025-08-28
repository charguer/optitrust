open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Variable.elim_reuse [cFunBody "f"; cVarDef "x"];
  !! Variable.elim_reuse [cFunBody "f"; cVarDef "c"];
  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Variable.elim_reuse [cFunBody "f"; cVarDef "y"]);

  !! Variable.elim_reuse [cFunBody "copy_back"; sInstr "b = a"];

  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Variable.elim_reuse [cFunBody "bad_write_after_copy_back"; cVarDef "b"]);

  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Variable.elim_reuse [cFunBody "bad_read_after_copy_back"; cVarDef "b"]);

  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Variable.elim_reuse [cFunBody "mutltiple_copy_backs"; cVarDef "b"]);

  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Variable.elim_reuse [cFunBody "resources_not_available"; cVarDef "x"]);
)
