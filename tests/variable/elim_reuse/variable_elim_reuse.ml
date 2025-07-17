open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Variable.elim_reuse [cFunBody "f"; cVarDef "x"];
  !! Variable.elim_reuse [cFunBody "f"; cVarDef "c"];
  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Variable.elim_reuse [cFunBody "f"; cVarDef "y"]);

  !! Variable.elim_reuse [cFunBody "g"; sInstr "b = a"];

  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Variable.elim_reuse [cFunBody "h"; cVarDef "b"]);

  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Variable.elim_reuse [cFunBody "i"; cVarDef "b"]);

  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Variable.elim_reuse [cFunBody "resources_not_available"; cVarDef "x"]);
)
