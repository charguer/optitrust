
open Ast


(* [apply_rule rule t] apply rule [rule] in the ast [t] *)
let apply_rule_aux (rule : rewrite_rule) (t : trm) : trm =
  let inst : tmap = Trm_matching.rule_match (rule.rule_vars  @ rule.rule_aux_vars) rule.rule_from t in
  let rule_before = rule.rule_to in
  let rule_after = Internal.subst inst rule_before in
  rule_after

let apply_rule (rule : rewrite_rule) : Target.Transfo.local =
  Target.apply_on_path (apply_rule_aux rule)

let compute_aux (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, ts) ->
    begin match (trm_prim_inv f), ts with
    | Some (Prim_unop Unop_get) , _ | Some (Prim_unop (Unop_bitwise_neg)), _ | Some (Prim_unop Unop_minus), _ | Some (Prim_unop (Unop_struct_access _)), _ | Some (Prim_unop (Unop_struct_get _)), _ -> t
    | Some (Prim_unop p), [t1] ->
      begin match trm_lit_inv t1 with
      | Some v1 -> compute_app_unop_value p v1
      | None -> t
      end
    | Some (Prim_binop Binop_and), [{desc = Trm_val (Val_lit (Lit_bool true));_}; t2] -> t2
    | Some (Prim_binop Binop_and), [{desc = Trm_val (Val_lit (Lit_bool false));_};_] -> trm_bool false
    | Some (Prim_binop Binop_and), [t2; {desc = Trm_val (Val_lit (Lit_bool true));_}] when trm_is_val_or_var t2 -> t2
    | Some (Prim_binop Binop_and), [t2;{desc = Trm_val (Val_lit (Lit_bool false));_}] when trm_is_val_or_var t2 -> trm_bool false
    | Some (Prim_binop Binop_or), [{desc = Trm_val (Val_lit (Lit_bool true));_}; _] -> trm_bool true
    | Some (Prim_binop Binop_or), [{desc = Trm_val (Val_lit (Lit_bool false));_}; _] -> trm_bool false
    | Some (Prim_binop Binop_or), [t2; {desc = Trm_val (Val_lit (Lit_bool true));_}] when trm_is_val_or_var t2 -> trm_bool true
    | Some (Prim_binop Binop_or), [t2; {desc = Trm_val (Val_lit (Lit_bool false));_}] when trm_is_val_or_var t2 -> t2
    | Some (Prim_binop p), [t1;t2] ->
      begin match (trm_lit_inv t1), (trm_lit_inv t2) with
      | Some v1, Some v2 -> compute_app_binop_value p v1 v2
      | _,_ -> t

      end
    | Some _ ,_ | None, _-> t

    end
  | _ -> t

let compute : Target.Transfo.local =
  Target.apply_on_path (compute_aux)
