open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
  !! Trace.apply Scope.infer_var_ids;
  !! Trace.apply Scope.unique_alpha_rename;
  !! Trace.apply Scope.infer_var_ids;
)