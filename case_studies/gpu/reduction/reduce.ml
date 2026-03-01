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

(* Create loop nest for kernel *)
let _ = Run.script_cpp_stage stage_ok (fun () ->
  !! Loop.tile (int (stride*tpb)) ~index:"bi" ~bound:TileDivides [cFor "i"];
  !! Loop.tile (int stride) ~index:"ti" ~bound:TileDivides [cFor "i"];
)

(* Parallelize first-level reduction: threads sum `stride` number of elements in parallel *)
let _ = Run.script_cpp_stage stage_ok (fun () ->
  parallelize_reduction ~temp_sums:"tile" "i" "ti" "sum";
  Marks.rem_all_marks_rec [];
)

(* Parallelize top-level reduction: blocks sum `tpb` elements in parallel *)
let _ = Run.script_cpp_stage stage_ok (fun () ->
  parallelize_reduction ~temp_sums:"partial_sums" "ti" "bi" "sum";
  !! Loop.hoist_alloc ~dest:[tBefore; occFirst; cFor "bi"] [cVarDef "tile"];
  Marks.rem_all_marks_rec [];
)

let replace_with_tree_reduce (logN: trm) (sum_instr: target) : unit =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    let seq_ind,loop_p = Path.index_in_surrounding_loop p in

    let lhs, rhs = trm_inv ~error:"expected set" trm_set_inv (Target.resolve_path p) in
    let arr_get,_ = trm_inv ~error:"expected plus" trm_add_inv rhs in
    let arr = trm_inv ~error:"expected get" trm_get_inv arr_get in

    let range,_,instrs,contract = trm_inv ~error:"expected for" trm_for_inv_instrs (Target.resolve_path loop_p) in

    let _ = Mlist.split (seq_ind+1) instrs in (* post_ghosts *)

    let arr' = trm_subst_var range.index (trm_int 0) arr in
    let arr_alias_v = new_var (fresh_var_name ~prefix:"reduce_arr_" ()) in
    let decl_arr_alias = trm_let (arr_alias_v, Option.unsome arr.typ) arr' in

    let sum_temp_v = new_var (fresh_var_name ~prefix:"sum_temp_" ()) in
    let tree_reduce_var,_ = find_var "tree_reduce" [] in
    let reduce_call = trm_let (sum_temp_v, typ_f32)
      (trm_apps (trm_var tree_reduce_var)
      [trm_var arr_alias_v; logN; range.stop]) in

    let write_sum = trm_set lhs (trm_var sum_temp_v) in

    let _,invariant = List.nth (contract.invariant.linear) 0 in
    let open Resource_formula in
    let invariant_formula = Pattern.pattern_match invariant [
      Pattern.(formula_points_to __ !__ __) (fun model () ->
        (fun t -> trm_subst_var range.index t model))
    ] in

    let v = new_var "v" in
    (* TODO: reuse the post_ghosts to make this not admitted;
    the problem is that they would rewrite lhs ~~> a(i) + inv(i) -> lhs ~~> inv(i+1), as opposed to directly
    giving the equality a(i) + inv(i) = inv(i+1). This makes it difficult to prove the equality reduce_sum(N,A) = inv(N) by induction.*)
    let rewrite = Resource_trm.ghost_rewrite_linear ~admitted:true ~typ:typ_f32 ~into:(invariant_formula range.stop) (formula_fun [v, typ_f32] (formula_points_to ~mem_typ:mem_typ_any lhs (trm_var v))) in

    Target.apply_at_path (fun _ ->
      trm_seq_helper ~braces:false [
        Trm decl_arr_alias;
        Trm reduce_call;
        Trm write_sum;
        Trm (Resource_trm.ghost rewrite);
      ]
    ) loop_p
  ) sum_instr)

(* Parallelize block-level reduction using tree reduction: *)
let _ = Run.script_cpp_stage stage_ok (fun () ->
  (* TODO: maybe this could be using includes instead? Or the user adds the include and then something can inline the include? *)
  !! Sequence.insert ~reparse:true (stmt (File.get_contents "/home/julien/work/optitrust/case_studies/gpu/reduction/tree_reduction.h"))[tBefore; cFunDef "reduce"];
  let sum_tg = [cFunDef "reduce"; cFor "bi"; cFor "ti"; cArrayWrite "partial_sums"] in
  !! Ghost.flatten_expr_rewrites (sum_tg @ [dRHS]);
  !! replace_with_tree_reduce (trm_int 7) sum_tg;
  !! Flags.with_flag Flags.check_validity false (fun () -> Function.inline_def [cFunDef "tree_reduce"]);

  (* random TODO: according to clang-format we are printing << as if it had a lower precedence than -, so there might be issues on reparse *)
)

