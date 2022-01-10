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
let parse_pattern (str : string) : (typed_vars * typed_vars *trm) =
  let output_file = "tmp_rule.cpp" in
  let splitted_pattern = Str.split (Str.regexp_string "==>") str in
  if List.length splitted_pattern < 2 then fail None "parse_pattern : could not split the given pattern, make sure that you are using ==> as a separator
    for the declaration of variables used in the pattern and the rule itself" ;
  let var_decls = String.trim (List.nth splitted_pattern 0) in
  let aux_var_decls, pat = if List.length splitted_pattern = 3 then (String.trim (List.nth splitted_pattern 1)),(List.nth splitted_pattern 2)
    else ("", List.nth splitted_pattern 1) in
  let var_decls_temp = Tools.fix_pattern_args var_decls in

  let aux_var_decls_temp = if aux_var_decls = "" then aux_var_decls else Tools.fix_pattern_args aux_var_decls in

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
        if Mlist.length tl1 < 1 then fail body.loc "parse_pattern: please enter a pattern of the shape var_decls ==> rule_to_apply";
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
        let aux_vars = List.filter_map (fun (x, ty) -> if Tools.pattern_matches x aux_var_decls then Some (x, ty) else None ) args in
        let pattern_vars = List.filter (fun (x, ty) -> not (List.mem (x, ty) aux_vars ) ) args in
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
let rule_match ?(higher_order_inst : bool = false ) (vars : typed_vars) (pat : trm) (t : trm) : tmap =

  (* [inst] maps each pattern variable to a term and to a type;
     when pattern variables are not yet instantiated,
     they are bound to the special term trm_uninitialized. *)
     (* LATER: we may need one day to introduce another special term Trm_uninstantiated  *)
  let inst = ref (List.fold_left (fun acc (x,ty) -> Trm_map.add x (ty, Ast.trm_uninitialized()) acc) Trm_map.empty vars) in
  let is_var (x : var) : bool =
    Trm_map.mem x !inst in
  let find_var (x : var) (u : trm) : unit =
    match Trm_map.find_opt x !inst with
    | None -> failwith "failure in rule_match: called find_var without first checking is_var"
    | Some (ty,t0) ->
        if Ast.is_trm_uninitialized t0 then
          inst := Trm_map.add x (ty,u) !inst
        else if not (Internal.same_trm ~ast_decode:false t0 u) then begin
          Tools.printf "Mismatch on variable '%s' already bound to '%s' which is not identical to '%s'.\n" x (Ast_to_c.ast_to_string t0) (Ast_to_c.ast_to_string u);
          Tools.printf "Witout encodings: '%s' is not identical to '%s'.\n" (Ast_to_c.ast_to_string ~ast_decode:false t0) (Ast_to_c.ast_to_string ~ast_decode:false u);
          Tools.printf "Locations: '%s' and '%s.'\n" (Ast.loc_to_string t0.loc) (Ast.loc_to_string u.loc);
          raise Rule_mismatch
        end
    in
  let _get_binding (x : var) : (var * typ) option =
    match Trm_map.find_opt x !inst with
    | None -> None
    | Some (ty,t0) -> match t0.desc with
       | Trm_var (_, y) -> Some (y,ty)
       | _ -> None
    in
  let with_binding ?(loc:location=None) (ty : typ) (x : var) (y : var) (f : unit -> unit) : unit =
     inst := Trm_map.add x (ty, trm_var ~loc y) !inst;
     f();
     inst := Trm_map.remove x !inst;
    (* Note: it would be incorrect to simply restore the map to its value before the call to [f],
       because other variables than [x] may have been instantiated during the call to [f]. *)
     in


  let rec aux (t1 : trm) (t2 : trm) : unit =
    let mismatch ?(t1:trm=t1) ?(t2:trm=t2) () : unit =
      Tools.printf "Mismatch on subterm, comparing '%s' with '%s'.\n" (Ast_to_c.ast_to_string t1) (Ast_to_c.ast_to_string t2);
      Tools.printf "Locations: '%s' and '%s.'\n" (Ast.loc_to_string t1.loc) (Ast.loc_to_string t2.loc);
      raise Rule_mismatch
      in
    let aux_list (ts1 : trms) (ts2 : trms) : unit =
      List.iter2 aux ts1 ts2 in
    (* [aux_with_bindings] is a function for matching two lists of terms,
       making sure to extend the [inst] map to take into account the
       variable names that are bound locally; for example,
         [int a = 3; return a]  should match  [int b = 3; return b]
      thus we need to introduce a binding from [a] to [b] using [with_binding]. *)
    let rec aux_with_bindings (ts1 : trms) (ts2 : trms) : unit =
      match ts1, ts2 with
      | [], [] -> ()
      | ({ desc = Trm_let (_vk1, (x1,t1), init1); _ } as dt1) :: tr1,
        ({ desc = Trm_let (_vk2, (x2,t2), init2); _ } as dt2) :: tr2 ->
           if not (same_types  (get_inner_ptr_type t1) (get_inner_ptr_type t2)) then begin
            Tools.printf "Type mismatch on trm_let\n";
            mismatch ~t1:dt1 ~t2:dt2 ()
          end;
          aux init1 init2;
          with_binding ~loc:dt2.loc t1 x1 x2 (fun () -> aux_with_bindings tr1 tr2)
      (* LATER: add support for Trm_let_fun, to allow matching local function definitions. *)
      | t1 :: tr1, t2 :: tr2 ->
          aux t1 t2;
          aux_with_bindings tr1 tr2
      | _ -> mismatch() (* note: in general, this should have been tested earlier on by comparing lengths *)
      in

    (* Check matching addressof annotation -- TODO: maybe we should simply ignore additions that appear on t2? *)
    if List.mem Address_operator t1.add then begin
      if not (List.mem Address_operator t2.add)
        then mismatch ~t1 ~t2 ();
      let remove_addressof (t:trm) : trm =
        { t with add = List.filter (fun a -> a <> Address_operator) t.add } in
      aux (remove_addressof t1) (remove_addressof t2)
    end else
    (* Else compare structures *)
    match t1.desc, t2.desc with

    (* Case for treating a match against a pattern variable *)
    | Trm_var (_, x), _ when is_var x -> find_var x t2

    (* Case for treating a match against a pattern such as [body(i)],
       where [body] is a pattern variable that corresponds to a function. *)
    | Trm_apps (({ desc = Trm_var (_, x); _} as trm_x), ts1), _ when higher_order_inst && is_var x ->
        let typ_args, typ_ret =
          match trm_x.typ with
          | None -> fail t1.loc (Printf.sprintf "rule_match: no type available for %s; try reparsing first" x)
          | Some ({typ_desc = Typ_fun (typ_args, typ_ret); _}) -> typ_args, typ_ret
          | _ -> fail t1.loc (Printf.sprintf "rule_match: the variable %s is used as a function but does not have a function type" x)
          in
        let msg1 i ti = fail None (Printf.sprintf "rule_match: the %d-th argument of the higher-order function variable %s is not a variable. It is the term: %s" i x (Ast_to_text.ast_to_string ti)) in
        let xargs = List.mapi (fun i ti -> match ti.desc with
          | Trm_var (_, x)
          (* LATER: find out if it is really correct to igore the get operation here *)
          | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [{desc = Trm_var (_, x); _}]) -> x
          | _ -> msg1 i ti) ts1 in
        (* DEPRECATED
          let msg2 i = fail t2.loc (Printf.sprintf "rule_match: the %d-th argument of the higher-order function variable %s is not found in the instantiation map" i x) in
          let targs = List.mapi (fun i xi -> match get_binding xi with Some typed_yi -> typed_yi | None -> msg2 i) xargs in *)
        if List.length typ_args <> List.length xargs
          then fail t2.loc (Printf.sprintf "rule_match: the function call does not have the same number of arguments as the higher-order function variable %s" x);
        let targs = List.combine xargs typ_args in
        let body = t2 in
        let func = trm_let_fun x typ_ret targs body in
        find_var x func

    | Trm_var (_, x1), Trm_var (_, x2) when x1 = x2 -> ()

    | Trm_val v1, Trm_val v2 when Internal.same_val v1 v2 -> ()

    | Trm_for (index1, start1, _direction1, stop1, step1, body1),
      Trm_for (index2, start2, _direction2, stop2, step2, body2) ->
        aux start1 start2;
        aux stop1 stop2;
        begin match step1, step2 with
        | Step stept1, Step stept2 -> aux stept1 stept2
        | _ -> if step1 <> step2 then mismatch()
        end;
        with_binding (typ_int()) index1 index2 (fun () -> aux body1 body2)

    | Trm_for_c (init1, cond1, step1, body1), Trm_for_c (init2, cond2, step2, body2) ->
        aux_with_bindings [init1; cond1; step1; body1] [init2; cond2; step2; body2]

    | Trm_seq tl1, Trm_seq tl2 ->
        if Mlist.length tl1 <> Mlist.length tl2 then mismatch();
        aux_with_bindings (Mlist.to_list tl1) (Mlist.to_list tl2)

    | Trm_apps (f1, ts1), Trm_apps (f2, ts2) ->
        aux f1 f2;
        aux_list ts1 ts2;

    | _ -> mismatch()
    in
  begin try aux pat t
  with Rule_mismatch ->
    Tools.printf "Mismatch comparing\n------\n%s\n------\n%s\n------\n" (Ast_to_c.ast_to_string ~ast_decode:false pat) (Ast_to_c.ast_to_string ~ast_decode:false t);
    raise Rule_mismatch
  end;
  Trm_map.map (fun (_ty,t) -> t) !inst

exception Rule_match_ast_list_no_occurrence_for of string


(* [tmap_to_list keys map] get the values of [keys] in map as a list *)
let tmap_to_list (keys : typed_vars) (map : tmap) : trms =
  List.map (fun (x, _) -> match Trm_map.find_opt x map with
    | Some v -> v
    | None -> raise (Rule_match_ast_list_no_occurrence_for x)
  ) keys

(* [tmap_filter_keys keys map] get a map with filtered keys *)
let tmap_filter_keys (keys : typed_vars) (map : tmap) : tmap =
  let keys = fst (List.split keys) in
  Trm_map.filter (fun k _ -> List.mem k keys) map

(* [rule_match_as_list pattern_vars pattern_instr t] returns the list of key values in the map generated from rule_match *)
let rule_match_as_list (pattern_vars : typed_vars) (pattern_instr : trm)  (t : trm) : trms =
  let inst : tmap = rule_match pattern_vars  pattern_instr t in
  List.map (fun (x,_) -> match Trm_map.find_opt x inst with
    | Some v -> v
    | None -> raise (Rule_match_ast_list_no_occurrence_for x)
  ) pattern_vars
