
open Ast

(* [parse_pattern str]: for a given pattern [str] return the list of variables used in that pattern
      and the ast of the pattern
      Example:
      str = "double a; double b; double c; (a + k * b) == (b * k + a)"
      Then this string will be splitted parsed as
      void f (double a, double b, double c) {
        (a + k * b) == (b * k + a)
      }
*)
let parse_pattern (str : string) : (vars * vars *trm) =
  let output_file = "tmp_rule.cpp" in
  let splitted_pattern = Str.split (Str.regexp_string "==>") str in
  if List.length splitted_pattern < 2 then fail None "parse_pattern : could not split the given pattern, make sure that you are using ==> as a separator
    for the declaration of variables used in the pattern and the rule itself" ;
  let var_decls = String.trim (List.nth splitted_pattern 0) in
  let aux_var_decls, pat = if List.length splitted_pattern = 3 then (String.trim (List.nth splitted_pattern 1)),(List.nth splitted_pattern 2)
    else ("", List.nth splitted_pattern 1) in
  
  let var_decls_temp  = String.mapi (fun i x ->
    if x = ';'
      then
        begin 
        if i <> String.length var_decls - 1
          then ','
          else ' '
        end
      else x) var_decls in

  let aux_var_decls_temp  = String.mapi (fun i x ->
    if x = ';'
      then
        if i <> String.length aux_var_decls - 1
          then ','
          else ' '
      else x) aux_var_decls in

  let fun_args = if aux_var_decls_temp = "" then var_decls_temp else var_decls_temp ^"," ^aux_var_decls_temp in
  let file_content = "bool f(" ^ fun_args ^ "){ \n" ^ "return " ^ pat ^ "\n}" in
  Xfile.put_contents output_file file_content;
  let _, ast_of_file = Trace.parse output_file in
  match ast_of_file.desc with
  | Trm_seq tl when (List.mem Main_file ast_of_file.annot) ->
    if Mlist.length tl = 0 then fail ast_of_file.loc "parse_pattern; couldn't parse pattern";
    let main_fun = Mlist.nth tl 0 in
    begin match main_fun.desc with
    | Trm_let_fun (_, _, args, body) ->
      begin match body.desc with
      | Trm_seq tl1 ->
        if Mlist.length tl1 < 1 then fail body.loc "parse_pattern: please enter a pattern of the shape var_decls # rule_to_appy";
        let pattern_instr_ret = Mlist.nth tl1 0 in
        let pattern_instr =
        begin match pattern_instr_ret.desc with
        | Trm_abort (Ret r1) ->
          begin match  r1 with
          | Some t1 -> t1
          | _ -> fail pattern_instr_ret.loc "parse_pattern: this should never appear"
          end
        | _ -> pattern_instr_ret
        end in
        let pattern_vars = fst (List.split args) in
        let aux_vars = List.filter_map (fun x -> if Internal.pattern_matches x aux_var_decls then Some x else None ) pattern_vars in
        let pattern_vars = List.filter (fun x -> not (List.mem x aux_vars ) ) pattern_vars in
        (pattern_vars, aux_vars, pattern_instr)
      | _ -> fail body.loc "parse_pattern: body of the function f should be a sequence"
      end
    | _ -> fail main_fun.loc "parse_pattern: the pattern was not entered correctly"
    end
  | _ -> fail ast_of_file.loc "parse_pattern: expected the main sequence of tmp_rule.cpp"


(* [parse_rule str]: for a given pattern [str] return a rewrite rule which is a record containing the
    the list of variables used in that rule, the rule itself and the result after applying that rule.
*)
let parse_rule (str : string) : rewrite_rule =
  let pattern_vars, aux_vars, pattern_instr = parse_pattern str in
  match pattern_instr.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_eq));_},[t1; t2]) ->
    {rule_vars = pattern_vars; rule_aux_vars = aux_vars; rule_from = t1; rule_to = t2}
  | _ -> fail pattern_instr.loc "parse_rule: could not parse the given rule"

exception Rule_mismatch

(* [rule_match]: for a given list of variables [vars] in pattern [pat] return a map with all the variables
      as keys and their associated ast as values
*)
let rule_match (vars : vars) (pat : trm) (t : trm) : tmap =
  let inst = ref Trm_map.empty in
  let rec aux (t1 : trm) (t2 : trm) : unit =
    let aux_list (ts1 : trms) (ts2 : trms) : unit =
      List.iter2 aux ts1 ts2 in
    match t1.desc, t2.desc with
    | Trm_var x, _ when List.mem x vars ->
      begin match Trm_map.find_opt x !inst with
      | None -> inst := Trm_map.add x t2 !inst
      | Some t0 when (Internal.same_trm t0 t2) -> ()
      | _ -> raise Rule_mismatch
      end

    | Trm_var x1, Trm_var x2 when x1 = x2 -> ()

    | Trm_val v1, Trm_val v2 when Internal.same_val v1 v2 -> ()
    | Trm_var _, Trm_val _ -> ()

    | Trm_apps (_f1, [ts1]), Trm_val _ ->
        if is_get_operation t1 then
        aux ts1 t2
        else ()
    | Trm_var _, Trm_apps (_f1, [ts2]) ->
        if is_get_operation t1 then
        aux t1 ts2
        else ()
    | Trm_apps (f1, ts1), Trm_apps (f2, ts2) ->
      let f1 = if is_get_operation t1 then (List.nth ts1 0) else f1 in
      let f2 = if is_get_operation t2 then (List.nth ts2 0) else f2 in
      begin
        aux f1 f2;
        aux_list ts1 ts2;
      end
    | _ ->
      Tools.printf "Comparing %s with %s" (Ast_to_c.ast_to_string t1) (Ast_to_c.ast_to_string t2);
      (* Tools.printf "Comparing %s with %s" (Ast_to_text.ast_to_string t1) (Ast_to_text.ast_to_string t2); *)
      raise Rule_mismatch
  in
  aux pat t;
  !inst

exception Rule_match_ast_list_no_occurrence_for of string

(* TODO:
  tmap_to_list (keys:vars) (map:tmap) : trms
  tmap_filter_keys (keys:vars) (map:tmap) : trmap

  in intro_pattern_array

    let inst = rule_match (pattern_vars @ pattern_aux_vars) pattern_instr t in
    let decls = tmap_to_list patterns_vars (tmap_filter_keys pattern_vars inst) in

*)



(* [rule_match_as_list pattern_vars pattern_instr t] returns the list of key values in the map generated from rule_match *)
let rule_match_as_list (pattern_vars : vars)  (pattern_instr : trm)  (t : trm) : trms =
  let inst : tmap = rule_match pattern_vars  pattern_instr t in
  List.map (fun x -> match Trm_map.find_opt x inst with
    | Some v -> v
    | None -> raise (Rule_match_ast_list_no_occurrence_for x)
  ) pattern_vars

(* [apply_rule rule t] apply rule [rule] in the ast [t] *)
let apply_rule_aux (rule : rewrite_rule) (t : trm) : trm =
  let inst : tmap = rule_match (rule.rule_vars  @ rule.rule_aux_vars) rule.rule_from t in
  let rule_before = rule.rule_to in
  let rule_after = Internal.variable_substitute inst rule_before in
  rule_after

let apply_rule (rule : rewrite_rule) : Target.Transfo.local =
  Target.apply_on_path (apply_rule_aux rule)

let compute_aux (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, ts) ->
    begin match (trm_prim_inv f), ts with
    | Some (Prim_unop Unop_get) , _ | Some (Prim_unop (Unop_bitwise_neg)), _ | Some (Prim_unop Unop_opp), _ | Some (Prim_unop (Unop_struct_field_addr _)), _ | Some (Prim_unop (Unop_struct_field_get _)), _ -> t
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



