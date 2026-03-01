open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important
let _ = Flags.display_includes := true

let _ = Run.script_cpp (fun () ->
  !! Show.add_marks_for_target_unit_tests [cInclude "tree_reduction.h"; cFunDef "tree_reduce"];
  !! Flags.with_flag Flags.check_validity false (fun () -> Function.inline_def [cInclude "tree_reduction.h"; cFunDef "tree_reduce"]);
  (*!! Gpu.convert_tail_thread_for [] [cFunDef "reduce2"; cFor "i"; cFor "t"];
  !! Loop.hoist ~inline:true [cFunDef "reduce2"; cVarDef "arr"];*)
)
