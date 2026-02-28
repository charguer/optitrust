open Optitrust
open Prelude

let _ = Flags.check_validity := true (* FIXME: false *)
let _ = Flags.use_resources_with_models := true
let _ = Flags.preserve_specs_only := true
let _ = Flags.pretty_matrix_notation := false
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important
let _ = Flags.only_big_steps := true

let _ = Run.script_cpp (fun () -> ())
let int = trm_int
let tpb = 128
let stride = 2

let stage_ok = fun i -> i = 4

let parallelize_reduction ?(temp_sums: string option) (inner_loop: string) (outer_loop: string) (sum_var: string): unit = begin
  bigstep (Printf.sprintf "parallelize reduction for %s,%s,%s" inner_loop outer_loop sum_var);
  let t = Option.unsome_or_else temp_sums (fresh_var_name ~prefix:"t") in
  let d = fresh_var_name ~prefix:"d" () in
  let t_scope = t ^ "_scope" in
  !! Sequence.intro ~mark:t_scope ~start:[tFirst; cForBody outer_loop] ~stop:[tLast; cForBody outer_loop] ();
  !! Variable.local_name ~var:sum_var ~local_var:t [cMark t_scope];
  !! Sequence.elim [cMark t_scope];
  !! Variable.insert ~name:d ~typ:typ_f32 ~value:(trm_get (trm_find_var sum_var [])) [cForBody outer_loop; tFirst];
  !! Accesses.shift_var ~inv:true ~factor:(trm_find_var d []) [cFor outer_loop; cVarDef t];
  !! Variable.inline [cVarDef d];
  !! Ghost.flatten_expr_rewrites [nbMulti; cWriteVar t; dRHS];
  !! Arith.simpl_surrounding_expr Arith.gather_rec [nbMulti; cVar sum_var];
  !! Resources.loop_minimize [occLast; cFor inner_loop];
  !! Loop.hoist [cVarDef t];
  !! Show.add_marks_for_target_unit_tests [tBefore; cFor outer_loop; cWriteVar sum_var];
  !! Loop.fission [tBefore; cFor outer_loop; cWriteVar sum_var];
end

let _ = Run.script_cpp_stage stage_ok (fun () ->
  !! Loop.tile (int (stride*tpb)) ~index:"bi" ~bound:TileDivides [cFor "i"];
  !! Loop.tile (int stride) ~index:"ti" ~bound:TileDivides [cFor "i"];
)

let _ = Run.script_cpp_stage stage_ok (fun () ->
  parallelize_reduction "i" "ti" "sum";
  Marks.rem_all_marks_rec [];
)

let _ = Run.script_cpp_stage stage_ok (fun () ->
  Show.add_marks_for_target_unit_tests [cPred (trm_has_cstyle RewriteSequence)];
  parallelize_reduction "ti" "bi" "sum";
  Marks.rem_all_marks_rec [];
)

let _ = Run.script_cpp_stage stage_ok (fun () ->
  !! Loop.hoist_alloc ~dest:[tBefore; occFirst; cFor "bi"] [cVarDef "t2"];
)
