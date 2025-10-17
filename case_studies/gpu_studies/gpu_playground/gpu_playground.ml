open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important

let _ = Run.script_cpp (fun () ->
  !! Omp.parallel_for [cFor "bx"];
  !! Omp.parallel_for [cFor "tx"];)
