open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
  !! Trace.apply Scope.unique_alpha_rename;
)
