open Prelude

type rewrite_rule = Trm_matching.rewrite_rule

(** [apply_rule_on rule t]: applies rule [rule] on trm [t]. *)
let apply_rule_on ~(error_msg : bool) ~(mark : mark) (rule : rewrite_rule) (t : trm) : trm =
  let inst : tmap = Trm_matching.rule_match ~error_msg (rule.rule_vars  @ rule.rule_aux_vars) rule.rule_from t in
  let rule_before = rule.rule_to in
  let rule_after = trm_subst inst rule_before in
  trm_add_mark mark rule_after

exception Rewrite_nomatch of string

let apply_rule_bottom_up ~(mark : mark) (rule : rewrite_rule) (t : trm) : trm =
  let nb_rewrites = ref 0 in
  let rec aux (t : trm) : trm =
    let t2 = trm_map aux t in
    try
      let t3 = apply_rule_on ~mark ~error_msg:false rule t2 in
      incr nb_rewrites;
      t3
    with
    | Trm_matching.Rule_mismatch -> t2
  in
  let t_res = aux t in
  if !nb_rewrites = 0 then raise (Rewrite_nomatch "no occurrence of the rewrite pattern was found inside the targeted term");
  t_res

(** [compute_on t]: applies arithmetic simplifications on trm [t]. *)
let compute_on (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, ts, _) ->
    begin match (trm_prim_inv f), ts with
    | Some (_, Prim_unop p), [t1] ->
      begin match trm_lit_inv t1 with
      | Some v1 ->
        begin match compute_app_unop_value p v1 with
        | Some lit -> trm_lit lit
        | None -> t
        end
      | None -> t
      end
    | Some (_, Prim_binop Binop_and), [{desc = Trm_lit (Lit_bool true);_}; t2] -> t2
    | Some (_, Prim_binop Binop_and), [{desc = Trm_lit (Lit_bool false);_};_] -> trm_bool false
    | Some (_, Prim_binop Binop_and), [t2; {desc = Trm_lit (Lit_bool true);_}] when trm_is_val_or_var t2 -> t2
    | Some (_, Prim_binop Binop_and), [t2;{desc = Trm_lit (Lit_bool false);_}] when trm_is_val_or_var t2 -> trm_bool false
    | Some (_, Prim_binop Binop_or), [{desc = Trm_lit (Lit_bool true);_}; _] -> trm_bool true
    | Some (_, Prim_binop Binop_or), [{desc = Trm_lit (Lit_bool false);_}; _] -> trm_bool false
    | Some (_, Prim_binop Binop_or), [t2; {desc = Trm_lit (Lit_bool true);_}] when trm_is_val_or_var t2 -> trm_bool true
    | Some (_, Prim_binop Binop_or), [t2; {desc = Trm_lit (Lit_bool false);_}] when trm_is_val_or_var t2 -> t2
    | Some (_, Prim_binop p), [t1;t2] ->
      begin match (trm_lit_inv t1), (trm_lit_inv t2) with
      | Some v1, Some v2 ->
        begin match compute_app_binop_value p v1 v2 with
        | Some lit -> trm_lit lit
        | None -> t
        end
      | _, _ -> t
      end
    | Some _ ,_ | None, _-> t
    end
  | _ -> t
