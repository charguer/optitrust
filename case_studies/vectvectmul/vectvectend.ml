open Optitrust
open Prelude

let _ = Flags.check_validity := false
let _ = Flags.preserve_specs_only := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important
(* let _ = Flags.save_ast_for_steps := Some Steps_all *)

let int = trm_int

let _ = Run.script_cpp (fun () ->
  !! Loop.fission [ cForBody "bi"; tBetweenAll ];
);
