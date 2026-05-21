open Optitrust
open Prelude

let _ = Flags.check_validity := true (* FIXME: false *)
(* let _ = Flags.use_resources_with_models := true *)
let _ = Flags.preserve_specs_only := true
let _ = Flags.pretty_matrix_notation := false
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_all (*Steps_important*)
(* let _ = Flags.save_ast_for_steps := Some Steps_all *)

let _ = Run.script_cpp (fun () ->
  !! Resources.ensure_computed ();
)
