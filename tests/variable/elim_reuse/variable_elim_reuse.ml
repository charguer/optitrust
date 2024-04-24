open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Variable.elim_reuse [cVarDef "x"];
  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Variable.elim_reuse [cVarDef "y"]);
)
