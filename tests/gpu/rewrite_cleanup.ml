open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.use_resources_with_models := true
let _ = Flags.preserve_specs_only := true

let stage_ok = fun i -> true

let _ = Run.script_cpp_stage stage_ok (fun () ->
  !! Accesses.shift_var ~factor:(trm_int 1) [nbMulti; cTopFunDef "test_var_inv"; cVarDef "s"];
  !! Show.add_marks_for_target_unit_tests [occFirst; cWriteVar "r"; dRHS];
  let p = Target.resolve_target_exactly_one [occFirst; cWriteVar "r"; dRHS] in
  let instr_p = Path.find_surrounding_instr p (Trace.ast ()) in
  let tg = [cPath instr_p] in
  !! Show.add_marks_for_target_unit_tests tg;
  !! Ghost.flatten_expr_rewrites [occFirst; cWriteVar "r"; dRHS ];
)
