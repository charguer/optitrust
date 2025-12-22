open Prelude
open AstParser

(* CHECK: #var-id, uses of x.name in this file *)

(** [parse_pattern pattern globdefs ctx ]: returns the list of variables used in [pattern] and the ast of that [pattern].
    [globdefs] - code entered as string that contains all the functions used in the pattern
    [ctx] - if set to true than the user doesn't need to provide the declarations of the functions used in pattern
            insted the function will take the current ast and dump it just before the pattern.
            this way all the functions used inside the pattern all well defined.
    Example:
    pattern = "double a; double b; double c; (a + k * b) == (b * k + a)"
    Then this string will be parsed as
    void f (double a, double b, double c) {
      (a + k * b) == (b * k + a)
    }
    Then the function returns ["a";"k";"b"] [] ((a + k * b) == (b * k + a)). *)
(* FIXME: This function is super weird since it parses code in C no matter which language is used in the original file *)
let parse_pattern ?(glob_defs : string = "") ?(ctx : bool = false) (pattern : string) : (typed_vars * typed_vars *trm) =
  let fix_pattern_args (var_decls : string) : string =
  let aux (var_decl : string) : string =
    let args_decl_list = Str.split (Str.regexp_string ",") var_decl in
    let first_var, other_vars = List.uncons args_decl_list in
    let var_type, _ =  List.unlast (Str.split (Str.regexp_string " ") first_var) in
    let var_type = List.fold_left (^) "" var_type in
    let other_vars = List.map (fun x -> var_type ^ " " ^ x) other_vars in
    let var_decl_list = first_var :: other_vars in
    (Tools.list_to_string ~sep:", " ~bounds:("","") var_decl_list)
    in
  let var_decls = Str.split (Str.regexp_string ";") var_decls in
  List.fold_left (fun acc x -> if acc = "" then acc ^ (aux x) else acc ^ "," ^ (aux x)) "" var_decls
   in

  let output_file = Filename.temp_file "tmp_rule" ".cpp" in
  let splitted_pattern = Str.split (Str.regexp_string "==>") pattern in
  if List.length splitted_pattern < 2 then failwith "Trm_matching.parse_pattern : could not split the given pattern, make sure that you are using ==> as a separator
    for the declaration of variables used in the pattern and the rule itself" ;
  let var_decls = String.trim (List.nth splitted_pattern 0) in
  let aux_var_decls, pat = if List.length splitted_pattern = 3 then (String.trim (List.nth splitted_pattern 1)),(List.nth splitted_pattern 2)
    else ("", List.nth splitted_pattern 1) in
  let var_decls_temp = fix_pattern_args var_decls in

  let aux_var_decls_temp = if aux_var_decls = "" then aux_var_decls else fix_pattern_args aux_var_decls in

  let fun_args = if aux_var_decls_temp = "" then var_decls_temp else var_decls_temp ^"," ^aux_var_decls_temp in

  let main_fun_str = "\nvoid f(" ^ fun_args ^ "){ \n" ^ pat ^ ";\n}" in
  let ctx_defs_orig = if ctx
    then begin
      let ast = Target.get_ast () in
      let ast2 = trm_seq_add_last (stmt main_fun_str) ast in
      let prefix = Filename.remove_extension output_file in
      Trace.output_prog (Style.style_for_reparse()) (Trace.get_context ()) prefix ast2;
      trm_main_inv_toplevel_defs ast
      (* Ast_to_c.ast_to_file output_file ast2; *)
    end else begin
      File.put_contents output_file main_fun_str;
      []
    end
  in

  let _, ast_of_file = Trace.parse ~persistant:false output_file in
  Sys.remove output_file;

  let defs = trm_main_inv_toplevel_defs ast_of_file in
  if defs = [] then trm_fail ast_of_file "Trm_matching.parse_pattern: couldn't parse pattern";
  let (ctx_defs, main_fun) = List.unlast defs in
  let ctx_vars_to_orig = Var_map.of_seq (Seq.zip
    (Seq.filter_map Trm.decl_name (List.to_seq ctx_defs))
    (Seq.filter_map (fun t -> Option.map (trm_var ?typ:t.typ) (Trm.decl_name t))
      (List.to_seq ctx_defs_orig))) in
  match trm_let_fun_inv main_fun with
  | Some (_, _, args, body, _) ->
    let aux_vars = List.filter_map (fun (x, ty) -> if Tools.pattern_matches x.name aux_var_decls then Some (x, ty) else None) args in
    let pattern_vars = List.filter (fun (x, ty) -> not (List.mem (x, ty) aux_vars)) args in
    let pattern_expr = Pattern.pattern_match body [
      Pattern.(trm_seq (mlist (!__ ^:: nil)) none) (fun t' () ->
        match trm_ignore_inv t' with
        | Some t' -> t'
        | None -> t'
      );
      Pattern.__ (fun () -> failwith "Trm_matching.parse_pattern: the pattern was not reparsed correctly")
    ] in
    (pattern_vars, aux_vars, trm_subst ctx_vars_to_orig pattern_expr)
  | _ -> trm_fail main_fun "Trm_matching.parse_pattern: the pattern was not entered correctly"

(** [rewrite_rule]: a type for defining rewrite rules *)
type rewrite_rule = {
  rule_vars : typed_vars;
  rule_aux_vars : typed_vars;
  rule_from : trm;
  rule_to : trm}

(** [parse_rule pattern]: returns a rewrite rule derived from [pattern](see [parse_pattern] ) which is a record containing
    the the list of  variables used in that rule,  the rule itself and the result after applying that rule. *)
let parse_rule ?(glob_defs : string = "") ?(ctx : bool = false) (pattern : string) : rewrite_rule =
  let pattern_vars, aux_vars, pattern_instr = parse_pattern ~glob_defs ~ctx pattern in
  match trm_eq_inv pattern_instr with
  | Some (t1, t2) ->
    {rule_vars = pattern_vars; rule_aux_vars = aux_vars; rule_from = t1; rule_to = t2}
  | _ ->
    trm_fail pattern_instr "Trm_matching.parse_rule: could not parse the given rule"

(** [Rule_mismatch]: exception raised by [rule_match] *)
exception Rule_mismatch

(** [normalize_trm]: Aims to reduce terms, especially matrix access, resulting in easier matching
Rules implemented:
- MINDEX reduction : reduct accesses of accesses into normalized form
Example : t[MINDEX2(m,n,i,0)][MINDEX1(n,j)] --> t[MINDEX2(m,n,i,j)]  *)
let normalize_trm (t : trm) : trm =

  match Matrix_trm.access_inv t with
  | Some (base_out, dims_out, inds_out) ->  (
      match Matrix_trm.access_inv base_out with
      | Some (base_in, dims_in, inds_in) ->
          let n_dims_out = List.length dims_out in
          if List.length dims_in >= n_dims_out then
            let last_dims_in = List.take_last n_dims_out dims_in in
            let zeros = List.init n_dims_out (fun i -> trm_int 0) in
            let left_list = last_dims_in @ List.take_last n_dims_out inds_in in
            let right_list = dims_out @ zeros in
            if List.fold_left2 (fun b t1 t2 -> b && Trm_unify.are_same_trm t1 t2) true left_list right_list then
              let indices = List.drop_last n_dims_out inds_in @ inds_out in
              Matrix_trm.access base_in dims_in indices
            else t
          else t
      | _ -> t)
  | _ -> t

(* Flag to report errors using low-level syntax *)
let debug_trm_matching = false

(** [rule_match ~higher_order_inst vars pat t]: LATER: Arthur  *)
let rule_match ?(higher_order_inst : bool = false) ?(error_msg = true) (vars : typed_vars) (pat : trm) (t : trm) : tmap =
  let error_msg = debug_trm_matching || error_msg in

  (** [inst] maps each pattern variable to an optional term and to a type;
     when pattern variables are not yet instantiated, the term is absent. *)
  let inst = ref (List.fold_left (fun acc (x,ty) -> Var_map.add x (ty, None) acc) Var_map.empty vars) in
  let is_var (x : var) : bool =
    Var_map.mem x !inst in
  let find_var (x : var) (u : trm) : unit =
    match Var_map.find_opt x !inst with
    | None -> failwith "Trm_matching.rule_match: called find_var without first checking is_var"
    | Some (ty, None) ->
      if debug_trm_matching then Tools.debug "Instantiating variable %s for the first time." (var_to_string x);
      inst := Var_map.add x (ty, Some u) !inst
    | Some (ty, Some t0) ->
      if not (Trm_unify.are_same_trm t0 u) then begin
        if error_msg then begin (* TODO: if + raise helper *)
          Tools.debug "Mismatch on variable '%s' already bound to '%s' which is not identical to '%s'." (var_to_string x)
            (Ast_to_c.ast_to_string ~optitrust_syntax:true t0) (Ast_to_c.ast_to_string ~optitrust_syntax:true u);
          Tools.debug "Witout encodings: '%s' is not identical to '%s'." (Ast_to_c.ast_to_string t0) (Ast_to_c.ast_to_string  u);
          (* TODO: debug type *)
          Tools.debug "Locations: '%s' and '%s'." (Ast.loc_to_string t0.loc) (Ast.loc_to_string u.loc);
        end;
        raise Rule_mismatch
      end else begin
        if debug_trm_matching then Tools.debug "Finding variable %s another time." (var_to_string x);
      end
    in
  let _get_binding (x : var) : (var * typ) option =
    match Var_map.find_opt x !inst with
    | Some (ty, Some t0) -> begin match t0.desc with
       | Trm_var y -> Some (y, ty)
       | _ -> None
        end
    | _ -> None
    in
  let with_binding ?(loc:location) (ty : typ) (x : var) (y : var) (f : unit -> unit) : unit =
     inst := Var_map.add x (ty, Some (trm_var ?loc ~typ:ty y)) !inst;
     f();
     inst := Var_map.remove x !inst;
    (* Note: it would be incorrect to simply restore the map to its value before the call to [f],
       because other variables than [x] may have been instantiated during the call to [f]. *)
     in


  let rec aux (t1 : trm) (t2 : trm) : unit =
    let mismatch ?(t1:trm=t1) ?(t2:trm=t2) () : unit =
      if error_msg then begin
        Tools.debug "Mismatch on subterm, comparing '%s' with '%s'." (Ast_to_c.ast_to_string t1) (Ast_to_c.ast_to_string t2);
        Tools.debug "Locations: '%s' and '%s'." (Ast.loc_to_string t1.loc) (Ast.loc_to_string t2.loc);
      end;
      raise Rule_mismatch
      in
    let aux_list (ts1 : trms) (ts2 : trms) : unit =
      if List.length ts1 <> List.length ts2 then mismatch ();
      List.iter2 aux ts1 ts2 in
    (** [aux_with_bindings] is a function for matching two lists of terms,
       making sure to extend the [inst] map to take into account the
       variable names that are bound locally; for example,
         [int a = 3; return a]  should match  [int b = 3; return b]
      thus we need to introduce a binding from [a] to [b] using [with_binding]. *)
    let rec aux_with_bindings (ts1 : trms) (ts2 : trms) : unit =
      match ts1, ts2 with
      | [], [] -> ()
      | ({ desc = Trm_let ((x1,t1), init1); _ } as dt1) :: tr1,
        ({ desc = Trm_let ((x2,t2), init2); _ } as dt2) :: tr2 ->
           if not (Trm_unify.are_same_trm (get_inner_ptr_type t1) (get_inner_ptr_type t2)) then begin
            Tools.debug "Type mismatch on trm_let";
            mismatch ~t1:dt1 ~t2:dt2 ()
          end;
          aux init1 init2;
          with_binding ?loc:dt2.loc t1 x1 x2 (fun () -> aux_with_bindings tr1 tr2)
      (* LATER: add support for Trm_let_fun, to allow matching local function definitions. *)
      | t1 :: tr1, t2 :: tr2 ->
          aux t1 t2;
          aux_with_bindings tr1 tr2
      | _ -> mismatch() (* note: in general, this should have been tested earlier on by comparing lengths *)
      in
    (* Here we add some specific rules for matching when matrix accesses occurs *)
    let t2 = normalize_trm t2 in
    (* if t2 : t[+]MINDEX2(n1,n2,i1,i2)  and t1: t'[+]MINDEX1(n,i)
      then t2 -> (t[+](MINDEX2(n1,n2,i1,0))(MINDEX1(n2,i2))
      Only works when mindex  is smaller  *)
    let t1, t2 =
    match (Matrix_trm.access_inv t1, Matrix_trm.access_inv t2) with
    | Some (base1, dims1, inds1), Some (base2, dims2, inds2)
      when List.length dims1 < List.length dims2 ->
        let length1 = List.length dims1 in
        let inds_in = List.drop_last length1 inds2 in
        let zeros = List.init length1 (fun i -> trm_int 0) in
        let access_in = Matrix_trm.access base2 dims2 (List.append inds_in zeros) in
        let access_out =
          Matrix_trm.access access_in (List.take_last length1 dims2) (List.take_last length1 inds2)
        in
        (t1, access_out)
    | _ -> (t1, t2) in

    match t1.desc, t2.desc with

    (* Case for treating a match against a pattern variable
     try_mindex let t2 = trm_access trm_access
     aux t1 t2'*)
    | Trm_var x, _ when is_var x -> find_var x t2

    (* Case for treating a match against a pattern such as [body(i)],
       where [body] is a pattern variable that corresponds to a function. *)
    | Trm_apps (({ desc = Trm_var x; _} as trm_x), ts1, _, _), _ when higher_order_inst && is_var x ->
        let typ_args, typ_ret =
          Pattern.pattern_match trm_x.typ [
            Pattern.(some (typ_fun !__ !__)) (fun typ_args typ_ret () -> (typ_args, typ_ret));
            Pattern.none (fun () ->
              trm_fail t1 (Printf.sprintf "Trm_matching.rule_match: no type available for %s; try reparsing first" (var_to_string x))
            );
            Pattern.(some __) (fun () ->
              trm_fail t1 (Printf.sprintf "Trm_matching.rule_match: the variable %s is used as a function but does not have a function type" (var_to_string x))
            )
          ]
        in
        let msg1 i ti = trm_fail t1 (Printf.sprintf "Trm_matching.rule_match: the %d-th argument of the higher-order function variable %s
                                     is not a variable.  It is the term: %s" i (var_to_string x) (Ast_to_text.ast_to_string ti)) in
        let xargs = List.mapi (fun i ti -> match ti.desc with
          | Trm_var x
          (* LATER: find out if it is really correct to igore the get operation here *)
          | Trm_apps ({desc = Trm_prim (_, Prim_unop Unop_get); _}, [{desc = Trm_var x; _}], _, _) -> x
          | _ -> msg1 i ti) ts1 in
        (* DEPRECATED
          let msg2 i = trm_fail t2 (Printf.sprintf "Trm_matching.rule_match: the %d-th argument of the higher-order function variable %s is not found in the instantiation map" i x) in
          let targs = List.mapi (fun i xi -> match get_binding xi with Some typed_yi -> typed_yi | None -> msg2 i) xargs in *)
        if List.length typ_args <> List.length xargs
          then trm_fail t2 (Printf.sprintf "Trm_matching.rule_match: the function call does not have the same number of arguments
                                            as the higher-order function variable %s" (var_to_string x));
        let targs = List.combine xargs typ_args in
        (* LATER ARTHUR: we need to replace "get p" by "p" for each argument "p" that did not have type const *)
        (* let body = t2 in *)
        (* ARTHUR: to be removed. *)
        let rec remove_get_operations_on_var_temporary (x : var) (t : trm) : trm = (*  *)
          match t.desc with
          | Trm_apps ({desc = Trm_prim (_, Prim_unop Unop_get)}, [{desc = Trm_var y;_}as ty], _, _) when y = x -> ty
          | _ -> trm_map (remove_get_operations_on_var_temporary x) t
        in
        let body = List.fold_left (fun tacc x ->
          remove_get_operations_on_var_temporary x tacc) t2 xargs in
        let func = trm_fun targs typ_ret body in
        find_var x func

    | Trm_var x1, Trm_var x2 when var_eq x1 x2 -> ()

    | (Trm_prim _, Trm_prim _ | Trm_lit _, Trm_lit _) when Trm_unify.are_same_trm t1 t2 -> ()

    | Trm_if (cond1, then1, else1),
      Trm_if (cond2, then2, else2) ->
        aux cond1 cond2;
        aux then1 then2;
        aux else1 else2

    | Trm_for (range1, mode1, body1, _),
      Trm_for (range2, mode2, body2, _) ->
        (* TODO: direction check wasn't here before? *)
        if range1.direction <> range2.direction || mode1 <> mode2 then mismatch();
        aux range1.start range2.start;
        aux range1.stop range2.stop;
        aux range1.step range2.step;
        with_binding typ_int range1.index range2.index (fun () -> aux body1 body2)

    | Trm_for_c (init1, cond1, step1, body1, _), Trm_for_c (init2, cond2, step2, body2, _) ->
        aux_with_bindings [init1; cond1; step1; body1] [init2; cond2; step2; body2]

    | Trm_seq (tl1, None), Trm_seq (tl2, None) ->
        (* TODO: manage sequence with results *)
        if Mlist.length tl1 <> Mlist.length tl2 then mismatch ();
        (* enlever les ghosts dans tl1,tl2 *)
        aux_with_bindings (Mlist.to_list tl1) (Mlist.to_list tl2)

    | Trm_apps (f1, ts1, _, _), Trm_apps (f2, ts2, _, _) ->
        aux f1 f2;
        aux_list ts1 ts2;

    | _ -> mismatch()
    in
  begin try aux pat t
  with Rule_mismatch ->
    if error_msg then begin
      Tools.debug "Mismatch comparing\n------\n%s\n------\n%s\n------" (Ast_to_c.ast_to_string ~optitrust_syntax:true pat) (Ast_to_c.ast_to_string ~optitrust_syntax:true t);
      if debug_trm_matching then begin
        let style = Ast_to_text.style_full in
        Tools.debug "With pattern variables: %s\n" (String.concat "," (List.map (fun (s,t) -> var_to_string s) vars));
        Tools.debug "Mismatch comparing\n------\n%s\n------\n%s\n------" (Ast_to_text.ast_to_string ~style pat) (Ast_to_text.ast_to_string ~style t);
      end
    end;
    raise Rule_mismatch
  end;
  Var_map.map (fun (_ty,t) -> match t with Some t -> t | None -> failwith "Trm_matching.rule_match: at least one pattern variable is not filled") !inst

(** [Rule_match_ast_list_no_occurrence_for]: exception raised by [tmap_to_list] *)
exception Rule_match_ast_list_no_occurrence_for of var


(** [tmap_to_list keys map]: gets the values of [keys] in [map] as a list *)
let tmap_to_list (keys : typed_vars) (map : tmap) : trms =
  List.map (fun (x, _) -> match Var_map.find_opt x map with
    | Some v -> v
    | None -> raise (Rule_match_ast_list_no_occurrence_for x)
  ) keys

(** [tmap_filter_keys keys map]: get a map with filtered keys *)
let tmap_filter_keys (keys : typed_vars) (map : tmap) : tmap =
  let keys = fst (List.split keys) in
  Var_map.filter (fun k _ -> List.mem k keys) map

(** [rule_match_as_list pattern_vars pattern_instr t]: returns the list of key values in the map generated from rule_match *)
let rule_match_as_list (pattern_vars : typed_vars) (pattern_instr : trm)  (t : trm) : trms =
  let inst : tmap = rule_match pattern_vars  pattern_instr t in
  List.map (fun (x,_) -> match Var_map.find_opt x inst with
    | Some v -> v
    | None -> raise (Rule_match_ast_list_no_occurrence_for x)
  ) pattern_vars
