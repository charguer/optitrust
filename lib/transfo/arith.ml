open Prelude
open Trm_unify
open Path
open Target
include Arith_basic


(** [simpl_surrounding_expr] first goes to the outside of the targeted expression,
   then applies [simpl] *)
let%transfo simpl_surrounding_expr ?(indepth : bool = true) (f : (expr -> expr)) (tg : target) : unit =
  Trace.tag_valid_by_composition ();
  let paths_to_simpl = ref Path_set.empty in
  let t = Trace.ast () in
  Trace.without_resource_computation_between_steps (fun () ->
    Target.iter (fun p ->
      paths_to_simpl := Path_set.add (snd (Path.find_arith_expr_root p t)) !paths_to_simpl;
    ) tg;
    Path_set.iter (fun p ->
      Arith_basic.simpl ~indepth f (target_of_path p);
    ) !paths_to_simpl
  )

(* TODO?
let default_simpl tg = simpl_surrounding_expr (fun x -> compute (gather x)) (nbAny :: tg)
*)
let default_simpl tg = simpl_surrounding_expr gather (nbAny :: tg)

let do_nothing tg = Marks.clean ~indepth:false (nbAny :: tg)

let arith_goal_solver ((x, formula): resource_item) (evar_ctx: Resource_computation.unification_ctx): Resource_computation.unification_ctx option =
  let open Resource_formula in
  let subst_ctx = Var_map.fold (fun var subst ctx ->
    match subst with
    | Resolved t -> Var_map.add var t ctx
    | Unknown _ -> ctx) evar_ctx Var_map.empty
  in
  let formula = trm_subst subst_ctx formula in
  let arith_solved = Pattern.pattern_match formula [
    Pattern.(trm_apps2 (trm_specific_var var_in_range) !__ (formula_range !__ !__ !__)) (fun index start stop step () ->
      Arith_core.(check_geq index start && check_lt index stop && check_eq (trm_trunc_mod_int index step) (trm_int 0))
    );
    Pattern.(trm_apps2 (trm_specific_var var_is_subrange) (formula_range !__ !__ !__) (formula_range !__ !__ !__)) (fun sub_start sub_stop sub_step start stop step () ->
      Arith_core.(check_geq sub_start start && check_leq sub_stop stop && check_eq (trm_trunc_mod_int sub_step step) (trm_int 0))
    );
    Pattern.(formula_is_true (trm_eq !__ !__)) (fun t1 t2 () -> check_eq t1 t2);
    Pattern.(formula_is_true (trm_neq !__ !__)) (fun t1 t2 () -> check_neq t1 t2);
    Pattern.(formula_is_true (trm_gt !__ !__)) (fun t1 t2 () -> check_gt t1 t2);
    Pattern.(formula_is_true (trm_ge !__ !__)) (fun t1 t2 () -> check_geq t1 t2);
    Pattern.(formula_is_true (trm_lt !__ !__)) (fun t1 t2 () -> check_lt t1 t2);
    Pattern.(formula_is_true (trm_le !__ !__)) (fun t1 t2 () -> check_leq t1 t2);
    Pattern.__ (fun () -> false)
  ] in
  if arith_solved then
    let evar_ctx = Var_map.add x (Resolved formula_arith_checked) evar_ctx in
    Some evar_ctx
  else
    None

let () = Resource_computation.pure_goal_solver := arith_goal_solver
