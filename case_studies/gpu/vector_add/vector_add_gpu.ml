open Optitrust
open Prelude
open Cuda_lowering

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := false
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important
let _ = Flags.cuda_codegen := true

(* Trace.apply (Cuda_lowering.lower_to_cuda)*)
let _ = Run.script_cpp (fun () ->
  Resources.ensure_computed ();)
