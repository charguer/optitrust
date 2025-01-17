open Ast
open Trm
open Typ
open Contextualized_error
open Resource_formula
open Resource_contract

(* TODO: Better module names:
  Resource_computation -> Resources.Computation
  Resources.compute = Resources.Computation.compute_resource *)

type pure_resource_set = resource_item list
type linear_resource_set = resource_item list
type nonrec unification_ctx = typ unification_ctx

let var_result = Resource_set.var_result

(**
   When instantating linear resources in a contract with X := Y substitutions, Y is a [Formula_inst.t].

   These formulas do not necessarily have stable var-ids.
  *)
module Formula_inst = struct
  type t = formula

  let inst_hyp (h: var): t = trm_var h
  let inst_hyp_inv = trm_var_inv

  let var_SplitRO = toplevel_var "SplitRO"

  let inst_split_read_only ~(new_frac: var) ~(old_frac: formula) (h: var) : t =
    trm_apps (trm_var var_SplitRO) [trm_var new_frac; old_frac; inst_hyp h]

  let inst_split_read_only_inv (f: t): (var * formula * var) option =
    Pattern.pattern_match_opt f [
      Pattern.(trm_apps3 (trm_var (var_eq var_SplitRO)) (trm_var !__) !__ (trm_var !__)) (fun frac old_frac hyp () -> (frac, old_frac, hyp))
    ]

  let var_ForgetInit = toplevel_var "ForgetInit"

  let inst_forget_init (h: var) : t =
    trm_apps (trm_var var_ForgetInit) [inst_hyp h]

  let inst_forget_init_inv (f: t): var option =
    Pattern.pattern_match_opt f [
      Pattern.(trm_apps1 (trm_var (var_eq var_ForgetInit)) (trm_var !__)) (fun h () -> h)
    ]

  let origin_hyp (f: t): var =
    match inst_hyp_inv f with
    | Some h -> h
    | None ->
      match inst_split_read_only_inv f with
      | Some (_, _, h) -> h
      | None ->
        match inst_forget_init_inv f with
        | Some h -> h
        | None -> failwith "Formula_inst.origin_hyp: unrecognized formula inst"
end

(** A type to distinguish between pure and linear resources *)
type resource_kind =
  | Pure
  | Linear

let resource_kind_to_string kind =
  match kind with
  | Pure -> "Pure"
  | Linear -> "Linear"


(** [update_usage hyp current_usage extra_usage] returns the usage resulting from using
    [current_usage] before using [extra_usage]. *)
let update_usage (hyp: var) (current_usage: resource_usage option) (extra_usage: resource_usage option): resource_usage option =
  match current_usage, extra_usage with
  | None, u -> u
  | u, None -> u
  | Some Required, Some Required -> Some Required
  | Some Ensured, Some Required -> Some Ensured
  | Some ArbitrarilyChosen, Some Required -> Some ArbitrarilyChosen
  | Some (Required | ArbitrarilyChosen | Ensured), Some (Ensured | ArbitrarilyChosen) -> failwith "Ensured resource %s share id with another one" (var_to_string hyp)
  | Some (Required | Ensured | ArbitrarilyChosen), _ | _, Some (Required | Ensured | ArbitrarilyChosen) ->
    failwith "Resource %s is used both as a pure and as a linear resource" (var_to_string hyp)
  | Some Produced, Some (ConsumedFull | ConsumedUninit) -> None
  | Some Produced, Some (SplittedFrac | JoinedFrac) -> Some Produced
  | _, Some Produced -> failwith "Produced resource %s share id with another one" (var_to_string hyp)
  | Some (ConsumedFull | ConsumedUninit), _ -> failwith "Consumed resource %s share id with another one" (var_to_string hyp)
  | Some JoinedFrac, Some ConsumedUninit -> Some ConsumedUninit
  | Some (SplittedFrac | JoinedFrac), Some (ConsumedFull | ConsumedUninit) -> Some ConsumedFull
  | Some JoinedFrac, Some JoinedFrac -> Some JoinedFrac
  | Some (SplittedFrac | JoinedFrac), Some (SplittedFrac | JoinedFrac) -> Some SplittedFrac

(** [update_usage_map ~current_usage ~extra_usage] returns the usage map resulting from using
    [current_usage] before using [extra_usage]. *)
let update_usage_map ~(current_usage: resource_usage_map) ~(extra_usage: resource_usage_map): resource_usage_map =
  Var_map.merge update_usage current_usage extra_usage

let update_usage_map_opt ~(current_usage: resource_usage_map option) ~(extra_usage: resource_usage_map option): resource_usage_map option =
  let open Option.Monad in
  let* current_usage in
  let* extra_usage in
  Some (update_usage_map ~current_usage ~extra_usage)

(** [add_usage hyp extra_usage usage_map] adds the [extra_usage] of hypothesis [hyp] to the [usage_map]. *)
let add_usage (hyp: var) (extra_usage: resource_usage) (usage_map: resource_usage_map): resource_usage_map =
  let current_usage = Var_map.find_opt hyp usage_map in
  match update_usage hyp current_usage (Some extra_usage) with
  | None -> Var_map.remove hyp usage_map
  | Some new_usage -> Var_map.add hyp new_usage usage_map

(** [rename_usage old_hyp new_hyp usage_map] rename the hypothesis [old_hyp] to [new_hyp] in the [usage_map]. *)
let rename_usage old_hyp new_hyp usage =
  match Var_map.find_opt old_hyp usage with
  | Some usage_kind -> Var_map.add new_hyp usage_kind (Var_map.remove old_hyp usage)
  | None -> usage

(** [used_set_to_usage_map res_used] converts the used resource set [res_used] into the corresponding usage map. *)
let used_set_to_usage_map (res_used: used_resource_set) : resource_usage_map =
  let pure_usage = List.fold_left (fun usage_map { inst_by } ->
      let used_fv = trm_free_vars inst_by in
      Var_set.fold (fun x -> Var_map.add x Required) used_fv usage_map)
    Var_map.empty res_used.used_pure
  in
  List.fold_left (fun usage_map { inst_by; used_formula } ->
    match Formula_inst.inst_split_read_only_inv inst_by with
    | Some (_, _, orig_hyp) -> Var_map.add orig_hyp SplittedFrac usage_map
    | None ->
      let hyp_usage = if formula_uninit_inv used_formula = None then ConsumedFull else ConsumedUninit in
      match Formula_inst.inst_hyp_inv inst_by with
      | Some hyp -> Var_map.add hyp hyp_usage usage_map
      | None ->
        match Formula_inst.inst_forget_init_inv inst_by with
        | Some hyp -> Var_map.add hyp hyp_usage usage_map
        | None -> failwith "Weird resource used"
  ) pure_usage res_used.used_linear

let new_fracs_from_used_set (res_used: used_resource_set): (var * formula) list =
  List.filter_map (fun { inst_by } ->
    match Formula_inst.inst_split_read_only_inv inst_by with
    | Some (frac, _, _) -> Some (frac, typ_frac)
    | None -> None
  ) res_used.used_linear

let formula_style = C_encoding.style_of_output_style (Style.default_style (* DEBUG: internal ~print_types:true *) ())
let formula_to_string = C_encoding.formula_to_string formula_style
let named_formula_to_string = C_encoding.named_formula_to_string formula_style
let filter_pure_resources = C_encoding.filter_pure_resources

(* FIXME: move printing out of this file. *)
let resource_list_to_string ?aliases res_list : string =
  String.concat "\n" (List.map (named_formula_to_string ?aliases) res_list)

let resource_set_to_string res : string =
  let spure = resource_list_to_string ~aliases:res.aliases (filter_pure_resources res.pure) in
  let slin = resource_list_to_string res.linear in
  Printf.sprintf "pure:\n%s\n\nlinear:\n%s\n" spure slin

let resource_set_opt_to_string res : string =
  Option.map_or resource_set_to_string "UnspecifiedRes" res

let unification_ctx_to_display_subst_ctx (evar_ctx: unification_ctx): trm varmap =
  Var_map.mapi (fun var subst ->
    match subst with Resolved t -> t | Unknown _ -> trm_var { var with name = ("?" ^ var.name) }
  ) evar_ctx

(** [MismatchingType (t, actual_typ, expected_typ)]: exception raised when [t] is of type [actual_typ] but is expected to be of type [expected_typ] *)
exception MismatchingType of trm * typ * typ

let raise_mismatching_type (t: trm) (actual_typ: typ) (expected_typ: typ) (evar_ctx: unification_ctx) =
  let subst_ctx = unification_ctx_to_display_subst_ctx evar_ctx in
  raise (MismatchingType (t, actual_typ, trm_subst subst_ctx expected_typ))

(** [Resource_not_found (kind, item, res_list)]: exception raised when the resource
   [item] is not found inside the resource list [res_list] *)
exception Resource_not_found of resource_kind * resource_item * resource_item list

let raise_resource_not_found (kind: resource_kind) ((name, formula): resource_item) (evar_ctx: unification_ctx) (inside: resource_item list) =
  let subst_ctx = unification_ctx_to_display_subst_ctx evar_ctx in
  let formula = trm_subst subst_ctx formula in
  let inside = List.map (fun (x, formula) -> (x, trm_subst subst_ctx formula)) inside in
  raise (Resource_not_found (kind, (name, formula), inside))

(** [compute_pure_typ env t] computes the type by recursively filling .typ fields of the pure / formula-convertible term [t] in a pure environment [env].
  Returns the type of [t] *)
let rec compute_pure_typ (env: pure_resource_set) ?(typ_hint: typ option) (t: trm): typ =
  let typ = match t.desc with
  | Trm_var v ->
    begin match Resource_set.find_pure v (Resource_set.make ~pure:env ()) with
    | Some typ -> typ
    | None when var_eq v Resource_trm.var_admitted ->
      (* LATER: Deal with __admitted in contracts in a cleaner way and disallow the case where neither t.typ not typ_hint are defined *)
      typ_or_auto (Option.or_ t.typ typ_hint)
    | None -> failwith "Variable '%s' could not be found in environment" (var_to_string v)
    end
  | Trm_lit l -> typ_of_lit l

  | Trm_fun (args, asked_rettyp, body, spec) ->
    if spec <> FunSpecUnknown then failwith "Pure functions cannot have specifications";

    (* Use the typ_hint to fill auto arguments types *)
    let args, rettyp_hint = match Option.bind typ_hint typ_pure_fun_inv with
    | Some (arg_typs, rettyp) ->
      begin match List.map2 (fun exp_arg_typ (x, decl_arg_typ) ->
        if is_typ_auto decl_arg_typ then (x, exp_arg_typ) else (x, decl_arg_typ)) arg_typs args
      with
      | args -> args, Some rettyp
      | exception Invalid_argument _ ->
        failwith "A function of type '%s' is expected but the function '%s' has arity %d" (Ast_to_c.typ_to_string (typ_pure_fun arg_typs rettyp)) (Ast_to_c.ast_to_string t) (List.length args)
      end
    | None -> args, None
    in
    (* TODO: Check types of arguments are well typed *)

    let rettyp = compute_pure_typ (env @ args) ?typ_hint:rettyp_hint body in
    if not (is_typ_auto asked_rettyp || are_same_trm rettyp asked_rettyp) then
      failwith "The function has a wrong return type (%s instead of %s)" (Ast_to_c.typ_to_string rettyp) (Ast_to_c.typ_to_string asked_rettyp);
    typ_pure_fun (List.map snd args) rettyp

  | Trm_prim (typ, prim) ->
    begin match prim with
    | Prim_unop unop ->
      begin match unop with
        | Unop_plus | Unop_minus ->
          if not (is_typ_numeric typ) then failwith "Cannot apply unary %s on a non numeric type" (Ast_to_c.ast_to_string (trm_prim typ prim));
          typ_pure_fun [typ] typ

        | Unop_bitwise_neg ->
          if not (is_typ_fixed_int typ) then failwith "Cannot apply unary ~ on a non fixed-size type";
          typ_pure_fun [typ] typ

        | Unop_neg ->
          if not (is_typ_bool typ) then failwith "Cannot apply negation operator on a non boolean";
          typ_pure_fun [typ] typ

        | Unop_cast dest_typ ->
          if not (is_typ_builtin typ) then failwith "Cannot apply cast from the non primitive type '%s'" (Ast_to_c.typ_to_string typ);
          if not (is_typ_builtin dest_typ) then failwith "Cannot apply cast to the non primitive type '%s'" (Ast_to_c.typ_to_string dest_typ);
          typ_pure_fun [typ] dest_typ

        | _ -> failwith "Unary operator %s is not a pure operator" (Ast_to_c.ast_to_string (trm_prim typ prim))
      end;

    | Prim_binop binop ->
      begin match binop with
        | Binop_add | Binop_sub | Binop_mul | Binop_exact_div ->
          if not (is_typ_numeric typ) then failwith "Cannot apply binary %s on a non numeric type" (Ast_to_c.ast_to_string (trm_prim typ prim));
          typ_pure_fun [typ; typ] typ

        | Binop_trunc_div | Binop_trunc_mod ->
          if not (is_typ_integer typ) then failwith "Cannot apply binary %s on a non numeric type" (Ast_to_c.ast_to_string (trm_prim typ prim));
          typ_pure_fun [typ; typ] typ

        | Binop_eq | Binop_neq | Binop_le | Binop_lt | Binop_ge | Binop_gt ->
          if not (is_typ_numeric typ) then failwith "Cannot apply binary %s on a non numeric type" (Ast_to_c.ast_to_string (trm_prim typ prim));
          typ_pure_fun [typ; typ] typ_bool

        | Binop_bitwise_and | Binop_bitwise_or | Binop_shiftl | Binop_shiftr ->
          if not (is_typ_integer typ) then failwith "Cannot apply binary %s on a non integer type" (Ast_to_c.ast_to_string (trm_prim typ prim));
          typ_pure_fun [typ; typ] typ

        | Binop_xor ->
          if not (is_typ_integer typ || is_typ_bool typ) then failwith "Cannot apply binary operator ^ on a non integer or boolean type" (Ast_to_c.ast_to_string (trm_prim typ prim));
          typ_pure_fun [typ; typ] typ

        | _ -> failwith "Binary operator %s is not a pure operator" (Ast_to_c.ast_to_string (trm_prim typ prim))
      end

      | _ -> failwith "Primitive operator %s is not a pure operator" (Ast_to_c.ast_to_string (trm_prim typ prim))
    end

  | Trm_apps (f, args, gargs) ->
    if gargs <> [] then failwith "Pure functions do not have ghost arguments";
    begin match f.desc, args with
    | Trm_prim (_, Prim_binop Binop_array_access), [arr; index] ->
      let arr_typ = compute_pure_typ env arr in
      let index_typ = compute_pure_typ env index in
      assert (is_typ_ptr arr_typ);
      assert (is_typ_integer index_typ);
      arr_typ
    | Trm_prim (_, Prim_unop Unop_struct_access field), [base] ->
      failwith "Unimplemented struct access typing"
    | Trm_var xf, [ptr] when var_eq xf var_cell ->
      let ptr_typ = compute_pure_typ env ptr in
      assert (is_typ_ptr ptr_typ);
      typ_hprop
    | Trm_var xf, [ptr; alloc_cells] when var_eq xf var_free ->
      let ptr_typ = compute_pure_typ env ptr in
      let alloc_cells_typ = compute_pure_typ env alloc_cells in
      assert (is_typ_ptr ptr_typ);
      assert (is_typ_hprop alloc_cells_typ);
      typ_hprop
    | Trm_var xf, _ when var_eq xf (Matrix_trm.mindex_var (List.length args / 2)) ->
      (* TODO: This should not be a special case. We should add a way to register pure arithmetic functions that can be used both in code and in specs *)
      assert ((List.length args) mod 2 = 0);
      List.iter (fun arg ->
        let arg_typ = compute_pure_typ env arg in
        if not (is_typ_integer arg_typ) then failwith "MINDEX should only be applied on integer arguments but here has argument '%s' of type '%s'" (Ast_to_c.ast_to_string arg) (Ast_to_c.typ_to_string arg_typ)
        ) args;
      typ_int
    | Trm_var xf, _ when var_eq xf typ_fun_var || var_eq xf typ_pure_fun_var ->
      List.iter (fun arg ->
        let arg_typ = compute_pure_typ env arg in
        assert (is_typ_sort arg_typ)
        ) args;
      typ_type
    | _ ->
      let f_typ = compute_pure_typ env f in
      match typ_pure_fun_inv f_typ with
      | Some (argstyp, rettyp) ->
        if List.length argstyp <> List.length args then
          failwith "Incorrect number of arguments when applying '%s'" (Ast_to_c.ast_to_string f);
        List.iter2 (fun arg expected_typ ->
            let arg_typ = compute_pure_typ env ~typ_hint:expected_typ arg in
            if not (are_same_trm arg_typ expected_typ) then begin
              (*Printf.printf "%s\n" (resource_list_to_string env);*)
              failwith "In pure expression '%s': argument '%s' has type '%s' where '%s' is expected" (Ast_to_c.ast_to_string t) (Ast_to_c.ast_to_string arg) (Ast_to_c.typ_to_string arg_typ) (Ast_to_c.typ_to_string expected_typ)
            end;
          ) args argstyp;
        rettyp
      | None -> failwith "Cannot apply a term that is not a pure function in a pure context"
    end
  | _ -> failwith "Non pure expression found in a pure context"
  in
  t.typ <- Some typ;
  typ

and compute_and_unify_typ (env: pure_resource_set) (t: trm) (expected_typ: typ) (evar_ctx: unification_ctx) =
  let actual_typ = compute_pure_typ env ~typ_hint:expected_typ t in
  match unify_trm actual_typ expected_typ evar_ctx (compute_and_unify_typ env) with
  | Some evar_ctx -> evar_ctx
  | None -> raise_mismatching_type t actual_typ expected_typ evar_ctx

let arith_goal_solver ((x, formula): resource_item) (evar_ctx: unification_ctx): unification_ctx option =
  let subst_ctx = Var_map.fold (fun var subst ctx ->
    match subst with
    | Resolved t -> Var_map.add var t ctx
    | Unknown _ -> ctx) evar_ctx Var_map.empty
  in
  let formula = trm_subst subst_ctx formula in
  let arith_solved = Pattern.pattern_match formula [
    Pattern.(trm_apps2 (trm_var (var_eq var_in_range)) !__ (formula_range !__ !__ !__)) (fun index start stop step () ->
      Arith_core.(check_geq index start && check_lt index stop && check_eq (trm_trunc_mod index step) (trm_int 0))
    );
    Pattern.(trm_apps2 (trm_var (var_eq var_is_subrange)) (formula_range !__ !__ !__) (formula_range !__ !__ !__)) (fun sub_start sub_stop sub_step start stop step () ->
      Arith_core.(check_geq sub_start start && check_leq sub_stop stop && check_eq (trm_trunc_mod sub_step step) (trm_int 0))
    );
    Pattern.(formula_is_true (trm_eq !__ !__)) (fun t1 t2 () -> Arith_core.check_eq t1 t2);
    Pattern.(formula_is_true (trm_neq !__ !__)) (fun t1 t2 () -> Arith_core.check_neq t1 t2);
    Pattern.(formula_is_true (trm_gt !__ !__)) (fun t1 t2 () -> Arith_core.check_gt t1 t2);
    Pattern.(formula_is_true (trm_ge !__ !__)) (fun t1 t2 () -> Arith_core.check_geq t1 t2);
    Pattern.(formula_is_true (trm_lt !__ !__)) (fun t1 t2 () -> Arith_core.check_lt t1 t2);
    Pattern.(formula_is_true (trm_le !__ !__)) (fun t1 t2 () -> Arith_core.check_leq t1 t2);
    Pattern.__ (fun () -> false)
  ] in
  if arith_solved then
    let evar_ctx = Var_map.add x (Resolved formula_arith_checked) evar_ctx in
    Some evar_ctx
  else
    None


(** [unify_pure (x, formula) res evar_ctx] unifies the given [formula] with one of the resources in [res].

   If none of the resources match, the optional goal solver is tried on the formula to try to discharge it.

   Also add a binding from [x] to the found resource in [evar_ctx].
   If it fails raise a {!Resource_not_found} exception. *)
let unify_pure ((x, formula): resource_item) (res: pure_resource_set) ?(goal_solver = arith_goal_solver) (evar_ctx: unification_ctx): unification_ctx =
  (* TODO: Add flag to disallow pure instantiation ? *)
  let find_formula formula (hyp_candidate, formula_candidate) =
    Option.map (fun evar_ctx -> Var_map.add x (Resolved (trm_var hyp_candidate)) evar_ctx)
      (unify_trm formula formula_candidate evar_ctx (compute_and_unify_typ res))
  in
  match List.find_map (find_formula formula) res with
  | Some evar_ctx -> evar_ctx
  | None ->
    match goal_solver (x, formula) evar_ctx with
    | Some evar_ctx -> evar_ctx
    | None -> raise_resource_not_found Pure (x, formula) evar_ctx res


(** [subtract_linear_resource_item ~split_frac (x, formula) res evar_ctx] subtracts [formula]
  from [res].
  Returns [(used, res', evar_ctx')] where:
  - [used] is the consumed resource item
  - [res'] is what remains from [res]
  - [evar_ctx'] is the evar context updated with new unification choices

  If [split_frac = true], then substracting [RO(?, R)] will only consume a fraction [g] of an
  [RO(f, R)] from [res]: [RO(g, R)] is consumed and [RO(f - g, R)] still remains.

  Depending on the formula of the resource, decide if we need a read-only fraction,
  a potentially uninitialized resource or a full ownership.

  Raises {!Not_found} if the [formula] cannot be consumed.
*)
let subtract_linear_resource_item ~(split_frac: bool) ((x, formula): resource_item)
  (res: linear_resource_set) (evar_ctx: unification_ctx) ~(pure_ctx: pure_resource_set)
  : used_resource_item * linear_resource_set * unification_ctx =
  let open Option.Monad in

  let rec extract
    (f : resource_item -> (used_resource_item * resource_item option * unification_ctx) option)
    (res: linear_resource_set)
    (evar_ctx: unification_ctx): used_resource_item * linear_resource_set * unification_ctx =
    match res with
    | [] -> raise Not_found (* caught later to create a Resource_not_found. *)
    | candidate :: res ->
      begin match f candidate with
      | Some (used_res, Some leftover, evar_ctx) -> (used_res, leftover :: res, evar_ctx)
      | Some (used_res, None, evar_ctx) -> (used_res, res, evar_ctx)
      | None ->
        let used, res, evar_ctx = extract f res evar_ctx in
        (used, candidate :: res, evar_ctx)
      end
  in

  let unify_and_remove_linear
    ((x, formula): resource_item)
    ~(uninit : bool) (* was [formula] surrounded by Uninit? *)
    (res: linear_resource_set)
    (evar_ctx: unification_ctx)
    ~(pure_ctx: pure_resource_set): used_resource_item * linear_resource_set * unification_ctx =
    (* Used by {!subtract_linear_resource_item} in the case where [formula] is not a read-only resource. *)
    (* LATER: Improve the structure of the linear_resource_set to make this
      function faster on most frequent cases *)
    extract (fun (candidate_name, formula_candidate) ->
      let used_formula, formula_to_unify =
        if uninit then
          match formula_uninit_inv formula_candidate with
          (* standard case: Uninit consumes Uninit *)
          | Some formula_candidate -> Formula_inst.inst_hyp candidate_name, formula_candidate
          (* coercion case: Uninit consumes Full *)
          | None -> Formula_inst.inst_forget_init candidate_name, formula_candidate
        else Formula_inst.inst_hyp candidate_name, formula_candidate
      in
      let* evar_ctx = unify_trm formula formula_to_unify evar_ctx (compute_and_unify_typ pure_ctx) in
      let used_formula = if uninit then formula_uninit formula_to_unify else formula_to_unify in
      Some (
        { hyp = x; inst_by = Formula_inst.inst_hyp candidate_name; used_formula },
        None,
        evar_ctx)
    ) res evar_ctx
  in

  let unify_and_split_read_only
    (hyp: var) ~(new_frac: var) (formula: formula)
    (res: linear_resource_set)
    (evar_ctx: unification_ctx)
    ~(pure_ctx: pure_resource_set): used_resource_item * linear_resource_set * unification_ctx =
    (* Used by {!subtract_linear_resource_item} in the case where [formula] is a read-only resource. *)
    (* LATER: Improve the structure of the linear_resource_set to make this
      function faster on most frequent cases *)
    extract (fun (h, formula_candidate) ->
      let { frac = cur_frac; formula = formula_candidate } = formula_read_only_inv_all formula_candidate in
      let* evar_ctx = unify_trm formula formula_candidate evar_ctx (compute_and_unify_typ pure_ctx) in
      Some (
        { hyp ; inst_by = Formula_inst.inst_split_read_only ~new_frac ~old_frac:cur_frac h; used_formula = formula_read_only ~frac:(trm_var new_frac) formula_candidate },
        Some (h, formula_read_only ~frac:(formula_frac_sub cur_frac (trm_var new_frac)) formula_candidate), evar_ctx)
    ) res evar_ctx
  in

  let formula, evar_ctx = unfold_if_resolved_evar formula evar_ctx in
  Pattern.pattern_match formula [
    (* special case where _Full disables split_frac. *)
    Pattern.(formula_read_only (trm_apps1 (trm_var (var_eq _Full)) !__) !__) (fun frac ro_formula () ->
      unify_and_remove_linear (x, formula_read_only ~frac ro_formula) ~uninit:false res evar_ctx ~pure_ctx
    );
    (* we split a fraction from an RO if we don't care about the fraction we get (evar). *)
    Pattern.(formula_read_only (trm_var !__) !__) (fun frac_var ro_formula () ->
      match Var_map.find_opt frac_var evar_ctx with
      | Some Unknown _ when split_frac ->
        (* Unknown fraction and split_frac: split a subfraction *)
        let new_frac, _ = new_frac () in
        let evar_ctx = Var_map.add frac_var (Resolved (trm_var new_frac)) evar_ctx in
        unify_and_split_read_only x ~new_frac ro_formula res evar_ctx ~pure_ctx
      | Some (Resolved frac) when are_same_trm frac full_frac ->
        (* evar_ctx tells we have RO(1, H): remove the RO wrapper *)
        unify_and_remove_linear (x, ro_formula) ~uninit:false res evar_ctx ~pure_ctx
      | _ ->
        (* Other cases: match the exact fraction *)
        unify_and_remove_linear (x, formula) ~uninit:false res evar_ctx ~pure_ctx
    );
    Pattern.(formula_uninit !__) (fun formula () ->
      unify_and_remove_linear (x, formula) ~uninit:true res evar_ctx ~pure_ctx);
    Pattern.__ (fun () ->
      unify_and_remove_linear (x, formula) ~uninit:false res evar_ctx ~pure_ctx)
  ]

(** [subtract_linear_resource_set ?split_frac ?evar_ctx ?pure_ctx res_from res_removed] subtracts [res_removed] from [res_from].
  Returns [(used, res', evar_ctx')] where:
  - [used] are the consumed resource items
  - [res'] is what remains from [res]
  - [evar_ctx'] is the evar context updated with new unification choices

  Raise [Resource_not_found] if one resource is missing.
  Use the unification environment [evar_ctx] for all resources in [res_removed]
  potentially instantiating evars inside.
  If [split_frac] is true, always try to give a smaller fraction than what is
  inside [res_from] for evar fractions.

  [pure_ctx] is the pure context in which variables from [res_from] are interpreted when performing unifications.
  It should be provided whenever [evar_ctx] is.
*)
let subtract_linear_resource_set ?(split_frac: bool = false) ?(evar_ctx: unification_ctx = Var_map.empty) ?(pure_ctx = []) (res_from: linear_resource_set) (res_removed: linear_resource_set)
  : used_resource_item list * linear_resource_set * unification_ctx =
  List.fold_left (fun (used_list, remaining_linear_res, evar_ctx) res_item ->
    try
      let used, res_from, evar_ctx = subtract_linear_resource_item ~split_frac res_item remaining_linear_res evar_ctx ~pure_ctx in
      (used :: used_list, res_from, evar_ctx)
    with Not_found ->
      raise_resource_not_found Linear res_item evar_ctx res_from
  ) ([], res_from, evar_ctx) res_removed

(** [partial_extract_linear_resource_set] computes the intersection between two linear resource sets
    If [evar_ctx] is provided, this is done up to evar unification.
  Returns [(used, res_left', res_right', evar_ctx')] where:
  - [used] are the consumed resource items
  - [res_left'] is what remains from [res_left]
  - [res_right'] is what remains from [res_right]
  - [evar_ctx'] is the evar context updated with new unification choices

  FIXME: not symmetrical left/right doc (uninit and unification)
*)
let partial_extract_linear_resource_set ?(evar_ctx: unification_ctx = Var_map.empty) ?(pure_ctx = []) (res_left: linear_resource_set) (res_right: linear_resource_set)
  : used_resource_item list * linear_resource_set * linear_resource_set * unification_ctx =
  List.fold_left (fun (used_list, res_left, unmatched_right, evar_ctx) res_item ->
    try
      let used, res_left, evar_ctx = subtract_linear_resource_item ~split_frac:false res_item res_left evar_ctx ~pure_ctx in
      (used :: used_list, res_left, unmatched_right, evar_ctx)
    with Not_found ->
      (used_list, res_left, res_item :: unmatched_right, evar_ctx)
  ) ([], res_left, [], evar_ctx) res_right


(** [specialize_and_extract_evars res inst_map ~inst_ctx]:
  specialize the pure resources with values from [inst_map] and extract the dominated evars from [res].

  Specialization checks types and can immediately resolve a dominated evar.
  Bindings from [inst_map] are typed inside [inst_ctx].

  An evar is dominated if its value will be implied by the instantation of other resources
  (i.e. it appears in the formula of another resource). *)
let specialize_and_extract_evars (res: resource_set) (inst_map: tmap) ~(inst_ctx: pure_resource_set): pure_resource_set * unification_ctx =
  let used_vars = Resource_set.used_vars res in
  let inst_map = ref inst_map in
  let evar_ctx = ref Var_map.empty in
  let pure_res = List.filter (fun (h, typ) ->
    match Var_map.find_opt h !inst_map with
    | Some t_inst ->
      inst_map := Var_map.remove h !inst_map;
      evar_ctx := Var_map.add h (Resolved t_inst) (compute_and_unify_typ inst_ctx t_inst typ !evar_ctx);
      false
    | None ->
      if Var_set.mem h used_vars then begin
        evar_ctx := Var_map.add h (Unknown typ) !evar_ctx;
        false
      end else begin
        true
      end
  ) res.pure in
  pure_res, !evar_ctx

open (Resource_set : sig exception Spec_not_found of var end)
exception NotConsumedResources of linear_resource_set
exception FractionConstraintUnsatisfied of formula * formula

type frac_quotient = { base: formula; num: int; den: int }

let rec frac_to_quotient (frac: formula) =
  Pattern.pattern_match frac [
    Pattern.(formula_frac_sub !__ !__) (fun base_frac removed_frac () ->
      let base_quot = frac_to_quotient base_frac in
      let removed_quot = frac_to_quotient removed_frac in
      if not (are_same_trm base_quot.base removed_quot.base) then raise_notrace Pattern.Next;
      let num =
        if base_quot.den = 1 && base_quot.num = 1 then
          removed_quot.den - removed_quot.num
        else if base_quot.den = removed_quot.den then
          base_quot.num - removed_quot.num
        else raise_notrace Pattern.Next
      in
      { removed_quot with num }
    );
    Pattern.(formula_frac_div !__ (trm_int !__)) (fun base_frac den () ->
      { base = base_frac; num = 1; den = den }
    );
    Pattern.__ (fun () ->
      { base = frac; num = 1; den = 1 }
    )
  ]

let check_frac_le subst_ctx (efrac, bigger_frac) =
  let efrac = Var_map.find efrac subst_ctx in
  let bigger_frac = trm_subst subst_ctx bigger_frac in
  let efrac_quot = frac_to_quotient efrac in
  let bigger_quot = frac_to_quotient bigger_frac in
  if not (are_same_trm efrac_quot.base bigger_quot.base) then raise (FractionConstraintUnsatisfied (efrac, bigger_frac));
  if not (bigger_quot.den = 1 && bigger_quot.num = 1 && efrac_quot.num <= efrac_quot.den) then begin
    if not (efrac_quot.den = bigger_quot.den) then raise (FractionConstraintUnsatisfied (efrac, bigger_frac));
    if not (efrac_quot.num <= bigger_quot.num) then raise (FractionConstraintUnsatisfied (efrac, bigger_frac));
  end


(** [extract_resources ~split_frac res_from subst_ctx res_to] checks that
  [res_from ==> res_to * H]
  in separation logic.

  Returns the leftover linear resources [H] along with the substitution context after
  instantiating ghost variables in [res_to].
  Effectively, this checks that all resources inside [res_to] can be built from resources inside
  [res_from] and returns the remaining linear resources after instantiation.
  Pure resources (ghosts) can be inferred using unification.

  If given, [subst_ctx] forces the instantiation of pure variables in [res_to].
  Pure resources in [subst_ctx] should be bound inside [res_to].

  TODO: Add unit tests for this specific function
*)
let extract_resources ~(split_frac: bool) (res_from: resource_set) ?(inst_map: tmap = Var_map.empty) (res_to: resource_set) : tmap * used_resource_set * linear_resource_set =
  let inst_map = Var_map.add var_result (trm_var var_result) inst_map in (* Add _Res := _Res to force unification of results from both sides together *)
  let inst_map = Var_map.map (trm_subst res_from.aliases) inst_map in
  let res_from = Resource_set.subst_all_aliases res_from in
  let not_specialized_pure_res_to, evar_ctx = specialize_and_extract_evars res_to inst_map ~inst_ctx:res_from.pure in

  let evar_ctx = Var_map.merge (fun x evar_ctx_binding alias_binding ->
    match evar_ctx_binding, alias_binding with
    | Some _, None -> evar_ctx_binding
    | None, Some alias_binding -> Some (Resolved alias_binding)
    | Some _, Some alias_binding ->
      if var_eq x var_result then Some (Resolved alias_binding)
      else failwith "Variable %s has an alias binding and is a unification variable at the same time" (var_to_string x)
    | None, None -> None) evar_ctx res_from.aliases in
  let res_from = Resource_set.subst_all_aliases res_from in
  if not (Var_map.is_empty res_to.aliases)
  then failwith "cannot extract resources into a resource set with aliases";

  let used_linear, leftover_linear, evar_ctx = subtract_linear_resource_set ~split_frac ~evar_ctx ~pure_ctx:res_from.pure res_from.linear res_to.linear in
  (* Prefer the most recently generated pure fact when a choice has to be made *)
  let available_pure = List.rev res_from.pure in
  let evar_ctx = List.fold_left (fun evar_ctx res_item ->
      unify_pure res_item available_pure evar_ctx) evar_ctx not_specialized_pure_res_to
  in

  (* All evars should have been instantiated at this point.
     There is a bug if it's not the case. *)
  let subst_ctx = Var_map.map (function
      | Resolved t -> t
      | Unknown _ ->
        let inst_failed_evars = Var_map.filter_map (fun evar resolution -> match resolution with Unknown typ -> Some typ | Resolved _ -> None) evar_ctx in
        let inst_failed_evars_str = String.concat ", " (List.map (fun (evar, typ) -> evar.name ^ ": " ^ Ast_to_c.typ_to_string typ) (Var_map.bindings inst_failed_evars)) in
        failwith "failed to instantiate evars %s" inst_failed_evars_str) evar_ctx
  in

  let used_pure = List.map (fun (hyp, formula) ->
      { hyp; inst_by = Var_map.find hyp subst_ctx; used_formula = trm_subst subst_ctx formula }
    ) res_to.pure in

  (* Check higher order function contracts in the post-condition *)
  ignore (Var_map.merge
            (fun fn_name spec_from spec_to ->
              match spec_from, spec_to with
              | _, None -> None (* We can drop extra specifications *)
              | None, Some _ -> raise (Resource_set.Spec_not_found fn_name)
              | Some spec_from, Some spec_to ->
                failwith "higher order functions are not yet implemented (found a spec for %s in pre-condition)" (var_to_string fn_name)
            )
            res_from.fun_specs res_to.fun_specs);

  (subst_ctx, { used_pure; used_linear }, leftover_linear)

(* FIXME: resource set intuition breaks down, should we talk about resource predicates? *)
(** [assert_resource_impl res_from res_to] checks that [res_from] ==> [res_to]. *)
let assert_resource_impl (res_from: resource_set) (res_to: resource_set) : used_resource_set =
  let _, used_res, leftovers = extract_resources ~split_frac:false res_from res_to in
  if leftovers <> [] then raise (NotConsumedResources leftovers);
  used_res

(** [compute_produced_resources subst_ctx contract_post] returns the resources produced
    by [contract_post] given the [subst_ctx] instantation of the contract. *)
let compute_produced_resources (subst_ctx: tmap) (contract_post: resource_set)
  : produced_resource_set =
  (* LATER: Manage higher order returned function *)
  let (contract_hyp_names, subst_ctx, aliases), pure =
    List.fold_left_map (fun (contract_names, subst_ctx, aliases) (h, formula) ->
        let produced_hyp = if var_eq h var_result then var_result else new_anon_hyp () in
        let contract_names = Var_map.add produced_hyp h contract_names in
        let produced_formula = trm_subst subst_ctx formula in
        let aliases = match Var_map.find_opt h contract_post.aliases with
          | Some alias -> Var_map.add produced_hyp (trm_subst subst_ctx alias) aliases
          | None -> aliases
        in
        let subst_ctx = if var_eq h var_result then subst_ctx else Var_map.add h (trm_var produced_hyp) subst_ctx in
        (contract_names, subst_ctx, aliases), (produced_hyp, produced_formula)
      ) (Var_map.empty, subst_ctx, Var_map.empty) contract_post.pure
  in
  let contract_hyp_names, linear =
    List.fold_left_map (fun contract_names (h, formula) ->
      let produced_hyp = new_anon_hyp () in
      let contract_names = Var_map.add produced_hyp h contract_names in
      let produced_formula = trm_subst subst_ctx formula in
      contract_names, (produced_hyp, produced_formula)
    ) contract_hyp_names contract_post.linear
  in
  let produced_res = { pure; linear; aliases; fun_specs = Var_map.empty } in
  { produced_res; contract_hyp_names }

(** Internal type to represent RO formula frac wands. *)
type frac_wand = formula * formula list
type frac_wand_list = (var * frac_wand) list
type frac_simplification_steps = (var * var) list

let rec formula_to_frac_wand (frac: formula) : frac_wand =
  Pattern.pattern_match frac [
    Pattern.(formula_frac_sub !__ !__) (fun sub_frac carved_frac () ->
      let (base_frac, carved_fracs) = formula_to_frac_wand sub_frac in
      (base_frac, carved_frac :: carved_fracs)
    );
    Pattern.__ (fun () -> (frac, []))
  ]

let frac_wand_to_formula ((base_frac, carved_fracs): frac_wand): formula =
  List.fold_right (fun carved_frac frac -> formula_frac_sub frac carved_frac) carved_fracs base_frac

(** [simplify_frac_wands frac_wands] try to simplify [frac_wands] on the same resource between themselves.

    For each frac_wand (g - h1 - .. - hn),
    - for each [hi] we try to join the current fraction with another one that has [hi] in the head position
    - then, once we are stuck, we look if [g] occurs in one of the minuses (carved frac) of the other wands
*)
let rec simplify_frac_wands (frac_wands: (var * frac_wand) list): frac_simplification_steps * frac_wand_list =
  let rec try_pop_base_frac (base_frac: formula) (frac_wands: frac_wand_list) =
    let open Option.Monad in
    match frac_wands with
    | [] -> None
    | (hyp, (wand_base_frac, carved_fracs)) :: frac_wands when are_same_trm base_frac wand_base_frac ->
      Some (frac_wands, hyp, carved_fracs)
    | non_matching_wand :: frac_wands ->
      let* frac_wands, hyp, carved_fracs = try_pop_base_frac base_frac frac_wands in
      Some (non_matching_wand :: frac_wands, hyp, carved_fracs)
  in

  let rec try_simplify_carved_fracs (carved_fracs: formula list) (frac_wands: frac_wand_list) =
    match carved_fracs with
    | [] -> [], frac_wands, []
    | carved_frac :: carved_fracs ->
      match try_pop_base_frac carved_frac frac_wands with
      | None ->
        let remaining_carved_fracs, frac_wands, consumed_hyps = try_simplify_carved_fracs carved_fracs frac_wands in
        carved_frac :: remaining_carved_fracs, frac_wands, consumed_hyps
      | Some (frac_wands, hyp, new_carved_fracs) ->
        let remaining_carved_fracs, frac_wands, consumed_hyps = try_simplify_carved_fracs (new_carved_fracs @ carved_fracs) frac_wands in
        remaining_carved_fracs, frac_wands, hyp :: consumed_hyps
  in

  let rec try_push_into_carved_frac base_frac new_carved_fracs frac_wands =
    let open Option.Monad in
    match frac_wands with
    | [] -> None
    | (hyp, (wand_base_frac, wand_carved_fracs)) as cur_wand :: frac_wands ->
      let rec try_push_into carved_fracs =
        match carved_fracs with
        | [] -> None
        | carved_frac :: other_carved_fracs when are_same_trm base_frac carved_frac ->
          Some (new_carved_fracs @ other_carved_fracs)
        | non_matching_carved_frac :: carved_fracs ->
          let* carved_fracs = try_push_into carved_fracs in
          Some (non_matching_carved_frac :: carved_fracs)
      in
      match try_push_into wand_carved_fracs with
      | Some carved_fracs -> Some (hyp, (hyp, (wand_base_frac, carved_fracs)) :: frac_wands)
      | None ->
        let* hyp, frac_wands = try_push_into_carved_frac base_frac new_carved_fracs frac_wands in
        Some (hyp, cur_wand :: frac_wands)
  in

  match frac_wands with
  | [] -> [], []
  | (hyp, (base_frac, carved_fracs)) :: frac_wands ->
    let remaining_carved_fracs, frac_wands, consumed_hyps = try_simplify_carved_fracs carved_fracs frac_wands in
    let simpl_steps = List.map (fun ch -> hyp, ch) consumed_hyps in
    match try_push_into_carved_frac base_frac remaining_carved_fracs frac_wands with
    | None ->
      let next_simpl_steps, frac_wands = simplify_frac_wands frac_wands in
      (simpl_steps @ next_simpl_steps, (hyp, (base_frac, remaining_carved_fracs)) :: frac_wands)
    | Some (target_hyp, frac_wands) ->
      let next_simpl_steps, frac_wands = simplify_frac_wands frac_wands in
      (simpl_steps @ (target_hyp, hyp) :: next_simpl_steps, frac_wands)

  (* LATER: ideas for a more efficient algorithm:
    we can use a table to cache the items to avoid repeated comparisons and traversals

    input items: (g1, [h11; ..; h1n]), (g2, [h21; ..; h2n])
    working map: [gi->[hi1; ..; hin]]
    auxiliary map: [hij->gi]
    result set: []
    while working map not empty
      we pop one item from working map, e.g. (g1, [h11; ..; h1n])
      remaining = []
      put the h1k in a queue
      while queue not empty,
        h1k next one in the queue
        remove auxiliary map form auxiliary map
        if working.mem h1k
          let [hj1...hjn] = working.pop h1k
          push the [hj1...hjn] into the queue
        else
          remaining.push h1k

      // look for g1 in others
      let gi = auxiliary.find_opt(his)
      | None -> result.push(g1k, remaining)
      | Some (his) ->
            working.push(gi, (List.remove g1 his) ++ remaining)
            List.iter his (fun ha -> auxiliary.push[ha -> gi]
  *)


(** [simplify_read_only_resources res] tries to simplify all read only resources from [res]
    joining all the fractions such that there is no pair of resources of the form
    [RO(h_i - f_1 - ... - f_n, H), RO(g - h_0 - ... - h_i - ... - h_n, H)]
 *)
let simplify_read_only_resources (res: linear_resource_set): (linear_resource_set * frac_simplification_steps) =
  let rec find_bucket_and_add formula hyp frac buckets =
    match buckets with
    | [] -> [formula, [hyp, formula_to_frac_wand frac]]
    | (other_formula, fracs) :: other_buckets when are_same_trm formula other_formula ->
      (other_formula, (hyp, formula_to_frac_wand frac) :: fracs) :: other_buckets
    | non_matching_bucket :: other_buckets ->
      non_matching_bucket :: (find_bucket_and_add formula hyp frac other_buckets)
  in
  let ro_buckets = ref [] in
  let non_ro_res = List.filter (fun (hyp, formula) ->
    match formula_read_only_inv formula with
    | None -> true
    | Some { formula; frac } ->
      ro_buckets := find_bucket_and_add formula hyp frac !ro_buckets;
      false
    ) res in
  let simpl_steps = ref [] in
  let ro_buckets = List.map (fun (formula, fracs) ->
      let new_simpl_steps, frac_wands = simplify_frac_wands fracs in
      simpl_steps := new_simpl_steps @ !simpl_steps;
      (formula, frac_wands)
    ) !ro_buckets
  in
  (* Here use a fold_right to maintain the initial ordering between RO resources.
     This is required for ghost_pair_chaining to pass, and maintaining the initial order
     is not a bad idea anyway.
     Note however that this function pulls RO ressources in the initial context in front. *)
  let res = List.fold_right (fun (formula, fracs) res ->
    List.fold_left (fun res (hyp, frac_wand) ->
      let frac = frac_wand_to_formula frac_wand in
      match trm_int_inv frac with
      | Some 1 -> (hyp, formula) :: res
      | _ -> (hyp, formula_read_only ~frac formula) :: res
      ) res fracs
  ) ro_buckets non_ro_res in
  (res, !simpl_steps)

(** [add_joined_frac_usage ro_simpl_steps usage] adds the usage
    corresponding to the read only simplifications [ro_simpl_steps] into the usage map [usage]. *)
let add_joined_frac_usage ro_simpl_steps usage =
  List.fold_left (fun used_set (joined_into, taken_from) ->
    let used_set = add_usage joined_into JoinedFrac used_set in
    let used_set = add_usage taken_from ConsumedFull used_set in
    used_set) usage ro_simpl_steps

(**
  [resource_merge_after_frame res_after frame] returns [res_after] * [frame] with simplifications.
  Cancels magic wands in [frame] with linear resources in [res_after] and
  returns the produced resource_set.

  Also returns a usage map that corresponds to the new usages from the production of resources in [res_after]
  and the resources merges.

  More precisely, the returned resource set must statifies the following invariants:
  [res_after] * [frame] ==> res
  there is no pair in res of the form RO(h_i - f_1 - ... - f_n, H), RO(g - h_0 - ... - h_i - ... - h_n, H)

  Ex: res_after.linear = RO('a, t)  and  frame = RO('b - 'a, t)
  gives res.linear = RO('b, t)
 *)
let resource_merge_after_frame (res_after: produced_resource_set) (frame: linear_resource_set) : resource_set * resource_usage_map * (var * var) list =
  let res_after = res_after.produced_res in
  let used_set = List.fold_left (fun used_set (h, _) -> Var_map.add h Ensured used_set) Var_map.empty res_after.pure in
  let used_set = List.fold_left (fun used_set (h, _) -> Var_map.add h Produced used_set) used_set res_after.linear in

  let linear, ro_simpl_steps = simplify_read_only_resources (res_after.linear @ frame) in
  let used_set = add_joined_frac_usage ro_simpl_steps used_set in

  { res_after with linear }, used_set, ro_simpl_steps

type resource_error_phase = ResourceComputation | PostconditionCheck

let resource_error_phase_to_string (phase: resource_error_phase) : string =
  match phase with
  | ResourceComputation -> "Resource computation"
  | PostconditionCheck -> "Postcondition check"

(** Exception used by [handle_resource_errors] to interrupt typing
    after a first error is discovered. *)
exception StoppedOnFirstError

(** List of errors accumulated during the current call to
    [trm_recompute_resources]. <private> *)
let global_errors : (resource_error_phase * exn) list ref = ref []

(** [ResourceError (t, e)] describes the fact that errors occurred
    during of typing, and [t] describes the partially typed
    term, and [e] describe the first error occurring at phase [p].
    Certain nodes [ti] in [t] may have their field [ti.errors]
    storing the error messages. *)
exception ResourceError of trm * resource_error_phase * exn

let _ = Printexc.register_printer (function
  | MismatchingType (t, actual_typ, expected_typ) ->
    Some (Printf.sprintf "Expression '%s' is of type '%s', but was expected of type '%s'" (Ast_to_c.ast_to_string t) (Ast_to_c.typ_to_string actual_typ) (Ast_to_c.typ_to_string expected_typ))
  | Resource_not_found (kind, item, context) ->
    Some (Printf.sprintf "%s resource not found:\n%s\nIn context:\n%s" (resource_kind_to_string kind) (named_formula_to_string item) (resource_list_to_string context))
  | Spec_not_found fn ->
    Some (Printf.sprintf "No specification for function %s" (var_to_string fn))
  | NotConsumedResources res ->
    Some (Printf.sprintf "Resources not consumed after the end of the block:\n%s" (resource_list_to_string res))
  | FractionConstraintUnsatisfied (efrac, bigger_frac) ->
    Some (Printf.sprintf "Fraction constraint unsatisfied: %s <= %s (currently we only reason about fractions of the form a - a/n - ... - a/n)" (formula_to_string efrac) (formula_to_string bigger_frac))
  | ResourceError (_t, phase, err) ->
    Some (Printf.sprintf "%s error: %s" (resource_error_phase_to_string phase) (Printexc.to_string err));
  | _ -> None)


type minimized_linear_triple = {
  new_fracs: resource_item list;
  linear_pre: resource_item list;
  linear_post: resource_item list;
  frame: resource_item list;
  usage_map: resource_usage_map;
}

type minimize_linear_triple_post_modif =
  | RemoveUnused
  | WeakenSplitFrac of { new_hyp: var; frac_before: formula; new_frac: var }
  | RemoveJoinedFrac of formula

(** [minimize_linear_triple linear_pre linear_post usage] removes and weakens resources from linear_pre and linear_post
    such that a term with usage map [usage] can still typecheck.
    Ideally it removes as much resources as possible, and put them in the [frame] field of the return value.
    While weakening resources new fractions might be generated and are stored in the [new_fracs] field of the return value.

    It assumes that [linear_pre] and especially [linear_post] have resource names that correspond to the usage map [usage]. *)
let minimize_linear_triple (linear_pre: resource_item list) (linear_post: resource_item list) (usage: resource_usage_map): minimized_linear_triple =
  let new_fracs = ref [] in
  let frame = ref [] in
  let post_modifs = ref Var_map.empty in
  let filter_linear_pre (hyp, formula) =
    match Var_map.find_opt hyp usage with
    | None ->
      post_modifs := Var_map.add hyp RemoveUnused !post_modifs;
      frame := (hyp, formula) :: !frame;
      None
    | Some (Required | Ensured | ArbitrarilyChosen) -> failwith "minimize_linear_triple: the linear resource %s is used like a pure resource" (var_to_string hyp)
    | Some Produced -> failwith "minimize_linear_triple: Produced resource %s has the same id as a contract resource" (var_to_string hyp)
    | Some ConsumedFull -> Some (hyp, formula)
    | Some ConsumedUninit ->
      begin match formula_uninit_inv formula with
      | Some _ -> Some (hyp, formula)
      | None -> Some (hyp, formula_uninit formula)
      end
    | Some SplittedFrac ->
      let { frac = frac_before; formula = base_formula } = formula_read_only_inv_all formula in
      let new_hyp = new_anon_hyp () in
      let frac_var, frac_ghost = new_frac () in
      new_fracs := frac_ghost :: !new_fracs;
      post_modifs := Var_map.add hyp (WeakenSplitFrac { new_hyp; frac_before; new_frac = frac_var }) !post_modifs;
      frame := (hyp, formula_read_only ~frac:(formula_frac_sub frac_before (trm_var frac_var)) base_formula) :: !frame;
      Some (new_hyp, formula_read_only ~frac:(trm_var frac_var) base_formula)
    | Some JoinedFrac ->
      let { frac = frac_before } = formula_read_only_inv_all formula in
      post_modifs := Var_map.add hyp (RemoveJoinedFrac frac_before) !post_modifs;
      frame := (hyp, formula) :: !frame;
      None
  in
  let linear_pre = List.filter_map filter_linear_pre linear_pre in

  let frac_wand_diff frac_before frac_after =
    let frac_base_before, frac_holes_before = formula_to_frac_wand frac_before in
    let frac_base_after, frac_holes_after = formula_to_frac_wand frac_after in
    if not (are_same_trm frac_base_before frac_base_after) then failwith "Incompatible base fraction during minimization (%s != %s)" (Ast_to_c.ast_to_string frac_base_before) (Ast_to_c.ast_to_string frac_base_after);
    let frac_holes_only_before = List.diff frac_holes_before frac_holes_after in
    let frac_holes_only_after = List.diff frac_holes_after frac_holes_before in
    (frac_holes_only_before, frac_holes_only_after)
  in
  let var_map_try_pop var map =
    match Var_map.find_opt var !map with
    | None -> None
    | Some x ->
      map := Var_map.remove var !map;
      Some x
  in
  let updated_usage_map = ref usage in
  let filled_frac_hole_to_read_only base_formula frac =
    let new_hyp = new_anon_hyp () in
    updated_usage_map := add_usage new_hyp Produced !updated_usage_map;
    (new_hyp, formula_read_only ~frac base_formula)
  in
  let filter_linear_post (hyp, formula) =
    match var_map_try_pop hyp post_modifs with
    | None -> [(hyp, formula)]
    | Some RemoveUnused -> []
    | Some WeakenSplitFrac { new_hyp; frac_before; new_frac } ->
      let { frac = frac_after; formula = base_formula } = formula_read_only_inv_all (formula_remove_uninit formula) in
      let frac_holes_only_before, frac_holes_only_after = frac_wand_diff frac_before frac_after in
      let carved_frac = frac_wand_to_formula (trm_var new_frac, frac_holes_only_after) in
      let remaining_fracs = List.map (filled_frac_hole_to_read_only base_formula) frac_holes_only_before in
      let carved_frac_hyp = new_anon_hyp () in
      updated_usage_map := add_usage carved_frac_hyp Produced !updated_usage_map;
      (carved_frac_hyp, formula_read_only ~frac:carved_frac base_formula) :: remaining_fracs
    | Some RemoveJoinedFrac frac_before ->
      let { frac = frac_after; formula = base_formula } = formula_read_only_inv_all (formula_remove_uninit formula) in
      let frac_holes_only_before, frac_holes_only_after = frac_wand_diff frac_before frac_after in
      assert (frac_holes_only_after = []);
      List.map (filled_frac_hole_to_read_only base_formula) frac_holes_only_before
  in
  let linear_post = List.concat_map filter_linear_post linear_post in
  if not (Var_map.is_empty !post_modifs) then failwith "minimize_linear_triple: missing resources that need to be patched in the post-condition";
  { new_fracs = List.rev !new_fracs; linear_pre; linear_post; frame = List.rev !frame; usage_map = !updated_usage_map }

(** <private> *)
let debug_print_computation_stack = false

(** Knowing that term [t] has contract [contract], and that [res] is available before [t],
  [compute_contract_invoc contract ?subst_ctx res t] returns the resources used by [t],
  and the resources available after [t].

  If provided, [subst_ctx] is a specialization context applied to [contract].
  Sets [t.ctx.ctx_resources_contract_invoc].
    *)
let compute_contract_invoc (contract: fun_contract) ?(inst_map: tmap = Var_map.empty) (res: resource_set) (t: trm): resource_usage_map * resource_set =
  if debug_print_computation_stack then
    Tools.debug "compute_contract_invoc:\n%s\n==>\n%s" (resource_set_to_string contract.pre) (resource_set_to_string contract.post);
  let subst_ctx, res_used, res_frame = extract_resources ~split_frac:true ~inst_map res contract.pre in

  let usage = used_set_to_usage_map res_used in
  let new_fracs = new_fracs_from_used_set res_used in
  let usage = List.fold_left (fun usage (frac, _) -> Var_map.add frac ArbitrarilyChosen usage) usage new_fracs in
  let res_produced = compute_produced_resources subst_ctx contract.post in

  let new_res, usage_after, ro_simpl_steps = resource_merge_after_frame res_produced res_frame in
  let new_res = { new_res with pure = new_fracs @ new_res.pure } in

  t.ctx.ctx_resources_contract_invoc <- Some {
    contract_frame = res_frame;
    contract_inst = res_used;
    contract_produced = res_produced;
    contract_joined_resources = ro_simpl_steps };

  let total_usage = update_usage_map ~current_usage:usage ~extra_usage:usage_after in
  Resource_set.(remove_useless_fracs total_usage (bind ~old_res:res ~new_res))

(** [handle_resource_errors t phase exn] hooks [exn] as error on the term [t],
    and save the fact that an exception [exn] was triggered. *)
let handle_resource_errors (t: trm) (phase:resource_error_phase) (exn: exn) =
  (* Save the error in the term where it occurred, or the referent (transitively) if any *)
  let error_str = sprintf "%s error: %s" (resource_error_phase_to_string phase) (Printexc.to_string exn) in
  let tref = trm_find_referent t in
  if !Flags.debug_errors_msg_embedded_in_ast then begin
    if tref != t
      then Tools.debug "GRABBING REFERENT FOR TERM:----\n%s\n-----" (Ast_to_c.ast_to_string t);
    Tools.debug "SAVING ERROR IN TERM:----\n%s\n-----\n%s-----" (Ast_to_c.ast_to_string tref) (Ast_to_text.ast_to_string tref);
  end;
  tref.errors <- error_str :: tref.errors;
  (*Tools.debug "ADDERROR %s\n  %s" error_str (Ast_to_c.ast_to_string t);*)
  (* Accumulate the error *)
  global_errors := (phase, exn) :: !global_errors;
  (* Interrupt if stop on first error *)
  if !Flags.stop_on_first_resource_error then Printexc.(raise_with_backtrace StoppedOnFirstError (get_raw_backtrace ()));
  (* Return empty resources maps as best effort to continue *)
  None, None


let empty_usage_map = Var_map.empty

let delete_stack_allocs instrs res =
  let extract_let_mut ti =
    match trm_let_inv ti with
    | Some (x, _, t) ->
      begin match trm_ref_any_inv t with
      | Some ty -> [formula_cells_var ty x]
      | None -> []
      end
    | None -> []
  in
  let to_free = List.concat_map extract_let_mut instrs in
  (*Tools.debug "Trying to free %s from %s\n" (String.concat ", " to_free) (resources_to_string (Some res));*)
  let res_to_free = Resource_set.make ~linear:(List.map (fun f -> (new_anon_hyp (), formula_uninit f)) to_free) () in
  let _, removed_res, linear = extract_resources ~split_frac:false res res_to_free in
  (removed_res, linear)

let check_resource_set_types ~(pure_ctx: pure_resource_set) (res: resource_set): pure_resource_set =
  let pure_ctx = List.fold_left (fun pure_ctx (pure_var, typ) ->
      let sort = compute_pure_typ pure_ctx typ in
      if not (is_typ_sort sort) then failwith "Pure resource '%s' has type '%s' that is not in Type or Prop" (var_to_string pure_var) (Ast_to_c.typ_to_string sort);
      pure_ctx @ [(pure_var, typ)]
    ) pure_ctx res.pure
  in
  List.iter (fun (lin_var, formula) ->
      let typ = compute_pure_typ pure_ctx formula in
      if not (is_typ_hprop typ) then failwith "Linear resource %s has a type that is not an HProp" (var_to_string lin_var);
    ) res.linear;
  (* LATER: Should check fun_contracts when higher order functions are supported *)
  assert (Var_map.is_empty res.fun_specs);
  assert (Var_map.is_empty res.aliases); (* Normally there is no aliases in contracts *)
  pure_ctx

let check_fun_contract_types ~(pure_ctx: pure_resource_set) (contract: fun_contract): unit =
  let pure_ctx = check_resource_set_types ~pure_ctx contract.pre in
  let _ = check_resource_set_types ~pure_ctx contract.post in
  ()

let find_prim_spec typ prim : typ * fun_spec_resource =
  let pure_prim prim =
    let pure_prim_typ = compute_pure_typ [] (trm_prim typ prim) in
    let argtyps, rettyp = trm_inv ~error:"Pure type of the pure operator should be a pure_fun" typ_pure_fun_inv pure_prim_typ in
    let func_typ = typ_fun argtyps rettyp in
    let args, pure_pre = List.fold_right (fun argtyp (args, pure_pre) ->
      let arg_var = new_anon_hyp () in
      (arg_var :: args, (arg_var, argtyp) :: pure_pre)
      ) argtyps ([], []) in
    let contract = { pre = Resource_set.make ~pure:pure_pre (); post = Resource_set.make ~pure:[var_result, rettyp] ~aliases:(Var_map.singleton var_result (trm_apps (trm_prim typ prim) (List.map trm_var args))) () } in
    func_typ, { args; contract; inverse = None }
  in
  let find_unop_spec (unop: unary_op) =
    match unop with
    | Unop_plus | Unop_minus | Unop_bitwise_neg | Unop_neg | Unop_cast _ ->
      pure_prim (Prim_unop unop)

    | Unop_address ->
      failwith "Address operator should have been eliminated at encoding phase"

    | Unop_pre_incr | Unop_pre_decr | Unop_post_incr | Unop_post_decr ->
      if not (is_typ_numeric typ) then failwith "Cannot apply unary %s on a non numeric type" (Ast_to_c.ast_to_string (trm_prim typ prim));
      let dest_var = new_hyp "dest" in
      let contract = {
        pre = Resource_set.make ~pure:[(dest_var, typ_ptr typ)] ~linear:[new_anon_hyp (), formula_cell_var dest_var] ();
        post = Resource_set.make ~pure:[var_result, typ] ~linear:[new_anon_hyp (), formula_cell_var dest_var] ()
      } in
      typ_fun [typ_ptr typ] typ, { args = [dest_var]; contract; inverse = None }

    | Unop_struct_access _ | Unop_struct_get _ -> failwith "Unsupported struct access typing"

    | Unop_get ->
      assert (is_typ_auto typ);
      (* typ = auto so we replace it by a fresh type variable *)
      let vartyp = new_hyp "T" in
      let typ = typ_var vartyp in
      let arg_var = new_hyp "p" in
      let frac_var = new_hyp "f" in
      let ro_cell = formula_read_only ~frac:(trm_var frac_var) (formula_cell_var arg_var) in
      (* With C++ syntax: template<typename T> T get(T* p) { __reads("p ~> Cell"); } *)
      let contract = {
        pre = Resource_set.make ~pure:[(vartyp, typ_type); (arg_var, typ_ptr typ); (frac_var, typ_frac)] ~linear:[new_anon_hyp (), ro_cell] ();
        post = Resource_set.make ~pure:[var_result, typ] ~linear:[new_anon_hyp (), ro_cell] ()
      } in
      (* func_typ is hard to give because this is a polymorphic function and we do not have polymorphic function types *)
      typ_auto, { args = [arg_var]; contract; inverse = None }
  in
  let find_binop_spec (binop: binary_op) =
    match binop with
    | Binop_add | Binop_sub | Binop_mul | Binop_exact_div | Binop_trunc_div | Binop_trunc_mod | Binop_eq | Binop_neq | Binop_le | Binop_lt | Binop_ge | Binop_gt | Binop_bitwise_and | Binop_bitwise_or | Binop_shiftl | Binop_shiftr | Binop_xor ->
      pure_prim (Prim_binop binop)

    | Binop_set ->
      assert (is_typ_auto typ);
      let vartyp = new_hyp "T" in
      let typ = typ_var vartyp in
      let dest_var = new_hyp "dest" in
      let value_var = new_hyp "value" in
      let contract = {
        pre = Resource_set.make ~pure:[(vartyp, typ_type); (dest_var, typ_ptr typ); (value_var, typ)] ~linear:[new_anon_hyp (), formula_uninit (formula_cell_var dest_var)] ();
        post = Resource_set.make ~linear:[new_anon_hyp (), formula_cell_var dest_var] ()
      } in
      typ_auto, { args = [dest_var; value_var]; contract; inverse = None }

    | Binop_array_access ->
      assert (is_typ_auto typ);
      let vartyp = new_hyp "T" in
      let typ = typ_var vartyp in
      let arr_var = new_hyp "arr" in
      let index_var = new_hyp "index" in
      let contract = {
        pre = Resource_set.make ~pure:[(vartyp, typ_type); (arr_var, typ_ptr typ); (index_var, typ_int)] ();
        post = Resource_set.make ~pure:[var_result, typ_ptr typ] ~aliases:(Var_map.singleton var_result (trm_array_access (trm_var arr_var) (trm_var index_var))) ()
      } in
      typ_auto, { args = [arr_var; index_var]; contract; inverse = None }

    | Binop_array_get ->
      assert (is_typ_auto typ);
      let vartyp = new_hyp "T" in
      let typ = typ_var vartyp in
      let arr_var = new_hyp "arr" in
      let index_var = new_hyp "index" in
      let frac_var = new_hyp "f" in
      let ro_cell = formula_read_only ~frac:(trm_var frac_var) (formula_cell (trm_array_access (trm_var arr_var) (trm_var index_var))) in
      let contract = {
        pre = Resource_set.make ~pure:[(vartyp, typ_type); (arr_var, typ_ptr typ); (index_var, typ_int); (frac_var, typ_frac)] ~linear:[new_anon_hyp(), ro_cell] ();
        post = Resource_set.make ~pure:[(var_result, typ)] ~linear:[new_anon_hyp (), ro_cell] ()
      } in
      typ_auto, { args = [arr_var; index_var]; contract; inverse = None }
  in
  match prim with
  | Prim_unop op -> find_unop_spec op
  | Prim_binop op -> find_binop_spec op
  | Prim_compound_assign_op op ->
    let pure_binop_typ = compute_pure_typ [] (trm_prim typ (Prim_binop op)) in
    if not (are_same_trm pure_binop_typ (typ_pure_fun [typ; typ] typ)) then
      failwith "Invalid compound assign operator";
    let dest_var = new_hyp "dest" in
    let operand_var = new_hyp "operand" in
    let contract = {
      pre = Resource_set.make ~pure:[(dest_var, typ_ptr typ); (operand_var, typ)] ~linear:[new_anon_hyp (), formula_cell_var dest_var] ();
      post = Resource_set.make ~linear:[new_anon_hyp (), formula_cell_var dest_var] ()
    } in
    typ_fun [typ_ptr typ; typ] typ_unit, { args = [dest_var; operand_var]; contract; inverse = None }

  | Prim_ref | Prim_new | Prim_ref_uninit | Prim_new_uninit ->
    let res_typ = typ_of_alloc typ in
    let alloc_cells = formula_cells_var typ var_result in
    let pure_pre, args_typ, args, alloc_res = match prim with
      | Prim_ref_uninit | Prim_new_uninit -> [], [], [], formula_uninit alloc_cells
      | _ ->
        let init_var = new_hyp "init_val" in
        [init_var, typ], [typ], [init_var], alloc_cells
    in
    let post_linear = match prim with
      | Prim_ref | Prim_ref_uninit -> [new_anon_hyp (), alloc_res]
      | _ -> [new_anon_hyp (), alloc_res; new_anon_hyp (), formula_free var_result alloc_cells]
    in
    let contract = {
      pre = Resource_set.make ~pure:pure_pre ();
      post = Resource_set.make ~pure:[var_result, res_typ] ~linear:post_linear ()
    } in
    typ_fun args_typ res_typ, { args ; contract; inverse = None }

  | Prim_delete ->
    assert (is_typ_auto typ);
    let var_typ = new_hyp "T" in
    let del_ptr = new_hyp "del_ptr" in
    let var_hprop = new_hyp "H" in
    let contract = {
      pre = Resource_set.make ~pure:[var_typ, typ_type; del_ptr, typ_ptr (typ_var var_typ); var_hprop, typ_hprop] ~linear:[new_anon_hyp (), formula_free del_ptr (trm_var var_hprop); new_anon_hyp (), formula_uninit (trm_var var_hprop)] ();
      post = Resource_set.make ()
    } in
    typ_auto, { args = [del_ptr]; contract; inverse = None }

(** [compute_resources ?expected_res res t] computes resources within [t], knowing that [res]
    resources are available before [t].
    Returns [(usage, res')].
    If successful, [usage] contains the resources used by [t] and [res'] the resources available
    after [t].
    If unsuccessful [usage = None] and [res' = expected_res].

    If provided, checks that [res' ==> expected_res].

    Sets [t.ctx.ctx_resources_*] fields in depth.
    *)
let rec compute_resources
  ?(expected_res: resource_set option)
  (res: resource_set option)
  (t: trm) : resource_usage_map option * resource_set option =
  if debug_print_computation_stack then Tools.debug "=====\nWith resources: %s\nComputing %s\n" (resource_set_opt_to_string res) (Ast_to_c.ast_to_string t);
  (* Define the referent for hooking type errors on existing terms
     when errors are triggered on terms that are generated on-the-fly. *)
  let referent : trm_annot =
    Trm.(trm_annot_set_referent trm_annot_default t) in
  t.ctx.ctx_resources_before <- res;
  let (let**) (type a) (x: a option) (f: a -> resource_usage_map option * resource_set option) =
    match x with
    | Some x -> f x
    | None -> None, None
  in
  (* Types subexpression [e] such that its effects are separated from the effects of other subexpressions. [res] is the typing context available for this subexpression.
    When typing succeeds, returns [Some (result_var, unused_res, post, usage_map)].
   *)
  let compute_subexpr_resources res e =
    match compute_resources (Some res) e with
    | Some usage_map, Some post_res ->
      let minimized_triple = minimize_linear_triple res.linear post_res.linear usage_map in
      let unused_res = { res with pure = res.pure @ minimized_triple.new_fracs; linear = minimized_triple.frame } in
      let minimized_post = Resource_set.(filter ~pure_filter:(pure_usage_filter usage_map keep_ensured) { post_res with linear = minimized_triple.linear_post }) in
      let result = new_anon_hyp () in
      let minimized_post = Resource_set.rename_var var_result result minimized_post in
      let usage_map = rename_usage var_result result minimized_triple.usage_map in
      Some (result, unused_res, minimized_post, usage_map)
    | _ -> None
  in

  t.typ <- None; (* By default we need to search for _Res, but we can skip this if with_result was called *)
  let with_result typ usage_map res =
    let res = Resource_set.push_back_pure (var_result, typ) res in
    let usage_map = Option.map (add_usage var_result Ensured) usage_map in
    t.typ <- Some typ;
    (usage_map, Some res)
  in

  let usage_map, res =
    let** res in
    try begin match t.desc with
    (* Values and variables are pure. *)
    | Trm_var x ->
      let typ = compute_pure_typ res.pure t in
      let res = Resource_set.add_alias var_result t res in
      with_result typ (Some (Var_map.singleton x Required)) res

    | Trm_lit Lit_unit ->
      t.typ <- Some typ_unit;
      Some Var_map.empty, Some res

    | Trm_lit _ ->
      let typ = compute_pure_typ res.pure t in
      let res = Resource_set.add_alias var_result t res in
      with_result typ (Some (Var_map.empty)) res

    | Trm_prim (typ, prim) ->
      let func_typ, spec = find_prim_spec typ prim in
      let res = Resource_set.add_fun_spec var_result spec res in
      with_result func_typ (Some (Var_map.empty)) res

    (* Defining a function is pure by itself, we check that the body satisfies the contract.
       If possible, we register a new function specification on [var_result], as well as potential inverse function metadata. *)
    | Trm_fun (args, rettyp, body, contract) ->
      let compute_resources_in_body contract =
        let body_res = Resource_set.bind ~old_res:res ~new_res:contract.pre in
        let body_usage, _ = compute_resources ~expected_res:contract.post (Some body_res) body in
        let bound_in_pre = Resource_set.bound_vars contract.pre in
        match body_usage with
        | Some body_usage -> Var_map.filter (fun used_var usage -> usage = Required && not (Var_set.mem used_var bound_in_pre)) body_usage
        | None -> Var_map.empty
      in
      begin match contract with
      | FunSpecContract contract ->
        (* Add arguments and return value to the contract *)
        let pre = List.fold_left (fun pre arg ->
            Resource_set.push_front_pure arg pre
          ) contract.pre (List.rev args)
        in
        let rettyp = if is_typ_auto rettyp then typ_unit else rettyp in
        let post = if is_typ_unit rettyp || Resource_trm.is_typ_ghost_ret rettyp
          then contract.post
          else Resource_set.push_front_pure (var_result, rettyp) contract.post
        in
        let contract = { pre ; post } in
        let contract = fun_contract_subst res.aliases contract in
        (* Typecheck the contract itself *)
        check_fun_contract_types ~pure_ctx:res.pure contract;
        let contract_usage = Var_map.of_seq (Seq.map (fun x -> (x, Required)) (Var_set.to_seq (fun_contract_free_vars contract))) in
        (* Compute resources recursively in the body *)
        let body_usage = compute_resources_in_body contract in
        (* Update the context with a binding _Res and the corresponding spec *)
        let args, argtyps = List.split args in
        let res = { res with fun_specs = Var_map.add var_result { args; contract; inverse = None } res.fun_specs } in
        let usage = update_usage_map ~current_usage:contract_usage ~extra_usage:body_usage in
        with_result (typ_fun argtyps rettyp) (Some usage) res
      | FunSpecReverts reverted_fn ->
        (* LATER: allow non empty arg list for reversible functions, this requires subtitution in the reversed contract *)
        assert (args = []);
        assert (Resource_trm.is_typ_ghost_ret rettyp || is_typ_auto rettyp);
        let reverted_spec = Var_map.find reverted_fn res.fun_specs in
        assert (reverted_spec.args = []);
        let reverse_contract = revert_fun_contract reverted_spec.contract in
        let body_usage = compute_resources_in_body reverse_contract in
        let fun_specs =
          res.fun_specs |>
          Var_map.add reverted_fn { reverted_spec with inverse = Some var_result } |>
          Var_map.add var_result { args = []; contract = reverse_contract; inverse = Some reverted_fn }
        in
        let usage = add_usage reverted_fn Required body_usage in
        with_result (typ_fun [] Resource_trm.typ_ghost_ret) (Some usage) { res with fun_specs }
      | FunSpecUnknown ->
        let argtyps = List.map snd args in
        with_result (typ_fun argtyps rettyp) (Some Var_map.empty) res
      end

    (* Transitively compute resources through all sequence instructions.
       At the end of the sequence, take into account that all stack allocations are freed. *)
    | Trm_seq (instrs, seq_result_var) ->
      let instrs = Mlist.to_list instrs in
      let usage_map, res =
        List.fold_left (fun (current_usage, res) instr ->
            (* Compute resource for one instruction *)
            let instr_usage, res = compute_resources res instr in
            (* There should be no binding of _Res for an instruction in a sequence *)
            assert (match res with
              | Some res -> Option.is_none (Resource_set.find_pure var_result res)
              | None -> true);
            let usage_map = update_usage_map_opt ~current_usage ~extra_usage:instr_usage in
            (usage_map, res))
          (Some Var_map.empty, Some res) instrs
      in

      (* Free the cells allocated with stack new *)
      let** res in
      let (removed_res, linear) = delete_stack_allocs instrs res in
      let usage_map = update_usage_map_opt ~current_usage:usage_map ~extra_usage:(Some (used_set_to_usage_map removed_res)) in
      let res = { res with linear } in

      begin match seq_result_var with
      | Some seq_result_var ->
        (* If it exist, use the variable bound as the result of the block *)
        let usage_map = Option.map (rename_usage seq_result_var var_result) usage_map in
        let res = Resource_set.rename_var seq_result_var var_result res in
        usage_map, Some res
      | None -> usage_map, Some res
      end

    (* First compute the resources of [body].
       Then, replace all mentions of [var_result] with [var]. *)
    | Trm_let ((var, typ), body) ->
      let usage_map, res_after = compute_resources (Some res) body in
      let** res_after in
      let result_typ = match Resource_set.find_pure var_result res_after with
        | Some res_typ -> res_typ
        | None -> failwith "No result binding inside the body of a let"
      in
      if not (are_same_trm (trm_subst res.aliases typ) result_typ || is_typ_auto typ) then
        failwith "Type of the let binding does not match type of the body (%s != %s)" (Ast_to_c.typ_to_string typ) (Ast_to_c.typ_to_string result_typ);
      let usage_map = Option.map (rename_usage var_result var) usage_map in
      let res_after = Resource_set.rename_var var_result var res_after in
      usage_map, Some res_after

    | Trm_record (_, fields) ->
      failwith "Unhandled record"
      (*(* TODO: factorize more subexpr logic with Trm_apps *)
      let** unused_res, post, usage_map =
        List.fold_left (fun acc ((_, v) : label option * trm) ->
          let open Option.Monad in
          let* res, current_post, current_usage = acc in
          let* _field_var, unused_res, extra_post, extra_usage = compute_subexpr_resources res v in
          let usage_map = update_usage_map ~current_usage ~extra_usage in
          let post = Resource_set.union current_post extra_post in
          Some (unused_res, post, usage_map)
        ) (Some (res, empty_resource_set, empty_usage_map)) (Mlist.to_list fields)
      in
      let unused_res = Resource_set.subst_all_aliases unused_res in (* Needed to simplify RO *)
      let res_after = Resource_set.union unused_res post in
      let linear_res_after, ro_simpl_steps = simplify_read_only_resources res_after.linear in
      let res_after = { res_after with linear = linear_res_after } in
      let usage_map = add_joined_frac_usage ro_simpl_steps usage_map in
      let usage_map, res_after = Resource_set.remove_useless_fracs usage_map res_after in
      Some usage_map, Some res_after*)

    (* TODO: try to factorize *)
    | Trm_apps (fn, effective_args, ghost_args) ->
      (* Retrieve a function specification in a given context *)
      begin try
        let** fn_var, res_after_fn, fn_post, fn_usage_map = compute_subexpr_resources res fn in
        let fn_ctx = Option.unsome fn.ctx.ctx_resources_after in
        let spec = Resource_set.find_result_fun_spec fn_ctx in

        (* If the function has a specification.
        Check that the [effective_args] use separate resources (order of evaluation does not matter).
        Build the instantation context with the known [effective_args] and [ghost_args].
        Then [compute_contract_invoc] to deduct used and remaining resources. *)

        let** inst_map, res_arg_frame, args_post, args_usage_map = try
          List.fold_left2 (fun acc contract_arg effective_arg ->
            let open Option.Monad in
            let* inst_map, unused_res, acc_post, usage_map = acc in
            let* arg_var, unused_res, new_post, arg_usage_map = compute_subexpr_resources unused_res effective_arg in
            let usage_map = update_usage_map ~current_usage:usage_map ~extra_usage:arg_usage_map in
            let acc_post = Resource_set.union new_post acc_post in
            let inst_map = Var_map.add contract_arg (trm_var arg_var) inst_map in
            Some (inst_map, unused_res, acc_post, usage_map)
          ) (Some (Var_map.empty, res_after_fn, fn_post, fn_usage_map)) spec.args effective_args
        with Invalid_argument _ ->
          failwith "Mismatching number of arguments for %s (expected %d and got %d)" (Ast_to_c.ast_to_string fn) (List.length spec.args) (List.length effective_args)
        in
        let res_after_args = Resource_set.union res_arg_frame args_post in
        let linear_res_after_args, ro_simpl_steps = simplify_read_only_resources res_after_args.linear in
        let res_after_args = { res_after_args with linear = linear_res_after_args } in
        let usage_map = add_joined_frac_usage ro_simpl_steps args_usage_map in

        let ghost_args_vars = ref Var_set.empty in
        let inst_map = List.fold_left (fun inst_map (ghost_var, ghost_inst) ->
          if Var_set.mem ghost_var !ghost_args_vars then (failwith "Ghost argument %s given twice for function %s" (var_to_string ghost_var) (Ast_to_c.ast_to_string fn));
          ghost_args_vars := Var_set.add ghost_var !ghost_args_vars;
          Var_map.add ghost_var ghost_inst inst_map) inst_map ghost_args
        in

        let call_usage_map, res_after = compute_contract_invoc spec.contract ~inst_map res_after_args t in
        let usage_map = List.fold_left (fun usage_map (_, ghost_inst) ->
            let ghost_inst_fv = trm_free_vars ghost_inst in
            Var_set.fold (fun x -> Var_map.add x Required) ghost_inst_fv usage_map
          ) usage_map ghost_args in
        let usage_map = update_usage_map ~current_usage:usage_map ~extra_usage:call_usage_map in

        let arg_ensured_vars = Var_set.of_seq (Seq.filter_map (fun (key, usage) -> match usage with
          | Ensured | ArbitrarilyChosen -> Some key
          | _ -> None) (Var_map.to_seq args_usage_map)) in
        let elim_pure_vars to_elim usage_map res =
          (* LATER: If access to a binding in context is in log(n), read usage maps to only check resources added during the call *)
          (* We cannot eliminate variables that occur in linear resources *)
          let to_elim = List.fold_left (fun to_elim (_, formula) ->
            Var_set.diff to_elim (trm_free_vars formula)) to_elim res.linear
          in
          let to_elim, rev_pure_res = List.fold_left (fun (to_elim, rev_pure_res) (x, ty) ->
              if Var_set.mem x to_elim then
                (to_elim, rev_pure_res)
              else if Var_set.is_empty (Var_set.inter to_elim (trm_free_vars ty)) then
                (to_elim, (x, ty) :: rev_pure_res)
              else
                (Var_set.add x to_elim, rev_pure_res)
            ) (to_elim, []) res.pure
          in
          let aliases = Var_map.filter (fun x v ->
              if Var_set.mem x to_elim then false
              else Var_set.is_empty (Var_set.inter to_elim (trm_free_vars v))
            ) res.aliases in
          let fun_specs = Var_map.filter (fun x spec ->
              if Var_set.mem x to_elim then false
              else
                (* Normally we should check if the contract refers to a variable in to_elim but it is too expansive *)
                (* This can cause problems only with complicated higer order contracts *)
                (* Var_set.is_empty (Var_set.inter to_elim (Resource_contract.fun_contract_free_vars spec.contract) *)
                true
            ) res.fun_specs in
          let usage_map = Var_map.filter (fun x _ -> not (Var_set.mem x to_elim)) usage_map in
          usage_map, { res with pure = List.rev rev_pure_res; aliases; fun_specs }
        in
        let usage_map, res_after = elim_pure_vars arg_ensured_vars usage_map res_after in

        Some usage_map, Some res_after

      with
      | Spec_not_found fn when var_eq fn Trm.var_sizeof ->
        begin match effective_args with
        | [ty_arg] ->
          (* LATER: Count the type as a required resource *)
          Some empty_usage_map, Some res
        | _ -> failwith "expected 1 argument for sizeof"
        end

      | Spec_not_found fn when var_eq fn Resource_trm.var_with_reverse ->
        begin match effective_args with
        | [fn; fn_rev] ->
          let usage, res = compute_resources (Some res) fn in
          let fn_var = new_anon_hyp () in
          let** res in
          let res = Resource_set.rename_var var_result fn_var res in
          let usage = Option.map (rename_usage var_result fn_var) usage in
          let fn_rev = match trm_fun_inv fn_rev with
          | Some ([], ret_typ, rev_body, _) ->
            trm_fun ~annot:referent [] ret_typ rev_body ~contract:(FunSpecReverts fn_var)
          | Some _ -> failwith "A reverse function should have no arguments"
          | None -> failwith "Second argument of __with_reverse should be a closure"
          in
          let extra_usage, res = compute_resources (Some res) fn_rev in
          let usage = update_usage_map_opt ~current_usage:usage ~extra_usage in
          let rev_fn_var = new_anon_hyp () in
          let** res in
          let res = Resource_set.rename_var var_result rev_fn_var res in
          let usage = Option.map (rename_usage var_result rev_fn_var) usage in
          let res = Resource_set.rename_var fn_var var_result res in
          let usage = Option.map (rename_usage fn_var var_result) usage in
          usage, Some res

        | _ -> failwith "expected 2 arguments for __with_reverse"
        end

      | Spec_not_found fn when var_eq fn Resource_trm.var_ghost_begin ->
        (* Checks that the called ghost is reversible, either because the argument is a __with_reverse pair,
           or because its reverse ghost is already in context.
           Then compute ressources as if it is a normal ghost call, and remember the instantiated contract.
           Store the reverse of the instantiated contract as the contract for _Res.
        *)
        let usage_map, res, contract_invoc = Pattern.pattern_match effective_args [
          Pattern.(!(trm_apps !__ nil __) ^:: nil) (fun ghost_call ghost_fn () ->
            let usage_map, res = compute_resources (Some res) ghost_call in
            let resources_after_ghost_fn = Option.unsome ghost_fn.ctx.ctx_resources_after in
            let spec = Resource_set.find_result_fun_spec  resources_after_ghost_fn in
            begin match spec.inverse with
            | Some _ -> ()
            | None -> failwith "%s is not reversible but is used inside __ghost_begin" (var_to_string fn)
            end;
            usage_map, res, ghost_call.ctx.ctx_resources_contract_invoc
          );
          Pattern.__ (fun () -> failwith "expected a ghost call inside __ghost_begin")
        ] in
        begin match res, contract_invoc with
        | Some res, Some invoc ->
          let inverse_pre = invoc.contract_produced.produced_res.linear in
          let inverse_post = List.map (fun { hyp; used_formula } -> (hyp, used_formula))
            invoc.contract_inst.used_linear
          in
          let inverse_spec = { args = [];
            contract = fun_contract_subst res.aliases { pre = Resource_set.make ~linear:inverse_pre (); post = Resource_set.make ~linear:inverse_post () };
            inverse = None }
          in
          usage_map, Some { res with pure = res.pure @ [var_result, Resource_trm.typ_ghost_fn]; fun_specs = Var_map.add var_result inverse_spec res.fun_specs }
        | _ -> failwith "Ghost call inside __ghost_begin should have generated a contract_invoc"
        end

      | Spec_not_found fn when var_eq fn Resource_trm.var_ghost_end ->
        (* Calls the closure made by GHOST_BEGIN and removes it from the pure context to ensure good scoping. *)
        Pattern.pattern_match effective_args [
          Pattern.(!(trm_var !__) ^:: nil) (fun fn fn_var () ->
            (* LATER: Maybe check that the variable is indeed introduced by __ghost_begin *)
            let usage_map, res = compute_resources (Some res) (trm_apps ~annot:referent fn []) in
            usage_map, Option.map (fun res -> { res with fun_specs = Var_map.remove fn_var res.fun_specs }) res
          );
          Pattern.__ (fun () -> failwith "__ghost_end expects a single variable as argument")
        ]

      | Spec_not_found fn when var_eq fn Resource_trm.var_assert_alias ->
        if effective_args <> [] then failwith "__assert_alias expects only ghost arguments";
        let ghost_call = trm_apps ~annot:referent (trm_var Resource_trm.var_assert_eq) [] ~ghost_args in
        let usage_map, res = compute_resources (Some res) ghost_call in
        let** res in

        let var = ref None in
        let subst = ref None in
        List.iter (fun (arg, value) -> match arg.name with
        | "x" when !var = None -> begin match trm_var_inv value with
          | Some x -> var := Some x
          | None -> failwith "__assert_alias expects a simple variable as first argument (got %s)" (Ast_to_c.ast_to_string value)
          end
        | "y" when !subst = None -> subst := Some value
        | _ -> ()) (ghost_args @ Option.value ~default:[] (Option.map (fun inv -> List.map (fun { hyp; inst_by } -> (hyp, inst_by)) inv.contract_inst.used_pure) ghost_call.ctx.ctx_resources_contract_invoc));
        let var = Option.get !var in
        let subst = trm_subst res.aliases (Option.get !subst) in (* Aliases never refer to other aliases *)
        begin match Var_map.find_opt var res.aliases with
        | None -> ()
        | Some alias when are_same_trm alias subst -> ()
        | _ -> failwith "Cannot add an alias for '%s': this variable already has an alias" (var_to_string var)
        end;
        let res = Resource_set.add_alias var subst res in
        usage_map, Some res

      | Spec_not_found fn when var_eq fn Resource_trm.var_admitted ->
        (* Stop the resource computation in the instructions following the call to __admitted()
           by forgetting the context without raising any error. *)
        (* FIXME: Admitted should only be allowed as a flag on contracts.
          This is dangerous for transformations that can take admitted as a normal instruction in a sequence *)
        None, None
      end

    (* Typecheck the whole for loop by instantiating its outer contract, and type the inside with the inner contract. *)
    | Trm_for (range, body, contract) ->
      let outer_contract = contract_outside_loop range contract in
      let usage_map, res_after = compute_contract_invoc outer_contract res t in

      let inner_contract = contract_inside_loop range contract in
      (* If the contract is not strict put all the framed resources inside the invariant *)
      let inner_contract, included_frame_hyps = if contract.strict then inner_contract, Var_set.empty else
        let frame = (Option.get t.ctx.ctx_resources_contract_invoc).contract_frame in
        (* Put the frame ressources at the end to make them used less often *)
        { pre = { inner_contract.pre with linear = inner_contract.pre.linear @ frame };
          post = { inner_contract.post with linear = inner_contract.post.linear @ frame } },
        List.fold_left (fun acc (x, _) -> Var_set.add x acc) Var_set.empty frame
      in
      let inner_usage, _ = compute_resources ~expected_res:inner_contract.post (Some (Resource_set.bind ~old_res:res ~new_res:inner_contract.pre)) body in

      let usage_map, res_after =
        match inner_usage with
        | Some inner_usage ->
          let extra_usage = Var_map.filter (fun x usage -> usage = Required || Var_set.mem x included_frame_hyps) inner_usage in
          let usage_map = update_usage_map ~current_usage:usage_map ~extra_usage in
          let res_after = if contract.strict then res_after else
            (* We need to change the ids of resources from the frame that were consumed inside the loop *)
            { res_after with linear = List.map (fun (x, f) -> match Var_map.find_opt x extra_usage with
              | Some (ConsumedFull | ConsumedUninit) -> (new_anon_hyp (), f)
              | _ -> (x, f)) res_after.linear }
          in
          Some usage_map, res_after
        | None -> None, res_after
      in

      t.typ <- Some typ_unit;
      usage_map, Some res_after

    (* Typecheck the 'then' and 'else' branches separately (including evaluating 'cond'),
       then try to join resources ('then' ==> 'else').
       *)
    | Trm_if (cond, then_branch, else_branch) ->
      let usage_cond, res_cond = compute_resources (Some res) cond in
      let** res_cond in
      let cond_hyp = new_anon_hyp () in
      let usage_cond = Option.map (rename_usage var_result cond_hyp) usage_cond in
      let res_cond = Resource_set.rename_var var_result cond_hyp res_cond in
      let cond_expr = Option.value ~default:(trm_var cond_hyp) (Var_map.find_opt cond_hyp res_cond.aliases) in
      let res_before_then = Resource_set.push_back_pure (new_anon_hyp (), bool_formula_to_prop cond_expr) res_cond in
      let res_before_else = Resource_set.push_back_pure (new_anon_hyp (), neg_bool_formula_to_prop cond_expr) res_cond in

      let usage_then, res_then = compute_resources (Some res_before_then) then_branch in
      let usage_else, res_else = compute_resources (Some res_before_else) else_branch in
      let** res_then in
      let** res_else in

      (* TODO: allow the user to customize the join resources *)
      (* TODO: be more clever about the synthetized join resources *)
      let res_join = Resource_set.make ~linear:(List.map (fun (_, formula) -> (new_anon_hyp (), formula)) res_else.linear) () in
      let used_join_then = assert_resource_impl res_then res_join in
      then_branch.ctx.ctx_resources_post_inst <- Some used_join_then;
      let used_join_else = assert_resource_impl res_else res_join in
      else_branch.ctx.ctx_resources_post_inst <- Some used_join_else;
      let usage_joined = match usage_then, usage_else with
        | Some usage_then, Some usage_else ->
          let usage_then_joined = update_usage_map ~current_usage:usage_then ~extra_usage:(used_set_to_usage_map used_join_then) in
          let usage_else_joined = update_usage_map ~current_usage:usage_else ~extra_usage:(used_set_to_usage_map used_join_else) in
          Some (Var_map.merge (fun x ut ue ->
              let on_conflict explanation =
                failwith "%s %s (%s / %s)" (var_to_string x) explanation (resource_usage_opt_to_string ut) (resource_usage_opt_to_string ue)
              in
              match ut, ue with
              | (Some Required, (Some Required | None)) | (None, Some Required) -> Some Required
              | (Some (Ensured | ArbitrarilyChosen) | None), (Some (Ensured | ArbitrarilyChosen) | None) -> None
              | (Some (Required | Ensured | ArbitrarilyChosen), _) | (_, Some (Required | Ensured | ArbitrarilyChosen)) ->
                on_conflict "mixed in pure and linear"
              | (None, _) | (_, None) ->
                on_conflict "consumed in a branch but not in the other"
              | (Some ConsumedFull, _) | (_, Some ConsumedFull) -> Some ConsumedFull
              | (Some ConsumedUninit, Some ConsumedUninit) -> Some ConsumedUninit
              | (Some (SplittedFrac | JoinedFrac), _) | (_, Some (SplittedFrac | JoinedFrac)) ->
                on_conflict "RO usage should have disappeared after join instantiation"
              | (Some Produced, _) | (_, Some Produced) ->
                on_conflict "is produced in one branch and is not in the join resource set")
            usage_then_joined usage_else_joined)
        | _, _ -> None
      in
      let res_usage = update_usage_map_opt ~current_usage:usage_cond ~extra_usage:usage_joined in
      let res_usage = Option.map (fun res_usage -> List.fold_left (fun used_set (h, _) -> Var_map.add h Produced used_set) res_usage res_join.linear) res_usage in

      res_usage, Some (Resource_set.bind ~old_res:res_cond ~new_res:res_join)

    | Trm_typedef typedef ->
      let res = Resource_set.push_back_pure (typedef.typedef_name, typ_type) res in
      begin match typedef.typedef_body with
      | Typedef_alias ty ->
        let sort = compute_pure_typ res.pure ty in
        if not (is_typ_type sort) then failwith "Type alias has type '%s' that is not in Type" (Ast_to_c.typ_to_string sort);
        let res = Resource_set.add_alias typedef.typedef_name ty res in
        let res_usage = Var_map.of_seq (Seq.map (fun x -> (x, Required)) (Var_set.to_seq (trm_free_vars ty))) in
        Some (Var_map.add typedef.typedef_name Ensured res_usage), Some res
      (* TODO: Add record handling *)
      | _ -> Some (Var_map.singleton typedef.typedef_name Ensured), Some res
      end

    | Trm_predecl _ ->
      (* For now we ignore predeclaration, LATER: manage them as let rec blocks *)
      Some Var_map.empty, Some res

    (* TODO: Refactor Trm_template to be able to handle generic functions *)
    | Trm_template (params, t) ->
      compute_resources (Some res) t

    | _ -> trm_fail t ("resource computation not implemented for " ^ Ast_to_c.ast_to_string t)
    end with
    | StoppedOnFirstError as e -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))
    | e -> handle_resource_errors t ResourceComputation e
  in

  t.ctx.ctx_resources_usage <- usage_map;
  if debug_print_computation_stack then
    Tools.debug "=====\nWith resources: %s\nWith usage: %s\nSaving %s\n"
      (resource_set_opt_to_string res)
      (Option.value ~default:"<unknown>" (Option.map (fun usage_map -> String.concat ", " (C_encoding.ctx_usage_map_to_strings usage_map)) usage_map))
      (Ast_to_c.ast_to_string t);
  t.ctx.ctx_resources_after <- res;
  (* Fill t.typ when not already set and available in res *)
  begin match res, t.typ with
  | Some res, None ->
    begin match Resource_set.find_pure var_result res with
    | Some typ -> t.typ <- Some typ
    | None -> t.typ <- Some typ_unit
    end
  | _ -> ()
  end;

  match res, expected_res with
  | Some res, Some expected_res ->
    (* Check that the expected resources after the expression are actually the resources available after the expression *)
    begin try
      (* LATER: add an annotation to provide post instantiation hints *)
      let used_res = assert_resource_impl res expected_res in
      t.ctx.ctx_resources_post_inst <- Some used_res;
      (* We need to account for pure usage inside the post instantiation, but filter out invariants that are passed to the next iteration without modification. *)
      let usage_map = Option.map (fun current_usage -> update_usage_map ~current_usage ~extra_usage:(used_set_to_usage_map { used_pure = List.filter (fun { hyp; inst_by } ->
          match trm_var_inv inst_by with
          | Some x -> not (var_eq x hyp)
          | None -> true
        ) used_res.used_pure; used_linear = [] })) t.ctx.ctx_resources_usage
      in
      t.ctx.ctx_resources_usage <- usage_map;
      usage_map, Some expected_res
    with
    | StoppedOnFirstError as e -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))
    | e ->
      ignore (handle_resource_errors t PostconditionCheck e);
      None, Some expected_res
    end
  | _, None -> usage_map, res
  | None, _ -> usage_map, expected_res


let rec trm_deep_copy (t: trm) : trm =
  let t = trm_map ~share_if_no_change:false trm_deep_copy t in
  t.ctx <- unknown_ctx (); (* LATER *)
  t

(* LATER: MINDEX should not be such a special case *)
let mindex_specs =
  List.fold_left (fun specs n ->
    let mindex_name = Matrix_trm.mindex_var n in
    let dims = List.map (fun i -> new_var (sprintf "N%s" (string_of_int i))) (List.range 1 n) in
    let indices = List.map (fun i -> new_var (sprintf "i%s" (string_of_int i))) (List.range 1 n) in
    let args = dims @ indices in
    let typed_args = List.map (fun arg -> (arg, typ_int)) args in
    let mindex_spec = { args; contract = {
      pre = Resource_set.make ~pure:typed_args ();
      post = Resource_set.make ~pure:[var_result, typ_int] ~aliases:(Var_map.singleton var_result (trm_apps (trm_var mindex_name) (List.map trm_var args))) ()
    }; inverse = None } in
    Var_map.add mindex_name mindex_spec specs
  ) Var_map.empty (List.range 0 Matrix_trm.max_nb_dims)

let ignore_spec =
  let vartyp = new_hyp "T" in
  let typ = typ_var vartyp in
  let arg_var = new_hyp "a" in
  (* With C++ syntax: template<typename T> void ignore(T a) { __pure(); } *)
  let contract = {
    pre = Resource_set.make ~pure:[(vartyp, typ_type); (arg_var, typ)] ();
    post = Resource_set.make ()
  } in
  { args = [arg_var]; contract; inverse = None }

let init_ctx = Resource_set.make ~pure:[
  typ_type_var, typ_type; (* Needed for polymorphic functions, do we create one metatype to avoid universe inconsistency ? *)
  typ_prop_var, typ_type;
  typ_hprop_var, typ_type;
  typ_unit_var, typ_type;
  typ_int_var, typ_type;
  typ_uint_var, typ_type;
  typ_isize_var, typ_type;
  typ_usize_var, typ_type;
  typ_i8_var, typ_type;
  typ_u8_var, typ_type;
  typ_i16_var, typ_type;
  typ_u16_var, typ_type;
  typ_i32_var, typ_type;
  typ_u32_var, typ_type;
  typ_i64_var, typ_type;
  typ_u64_var, typ_type;
  typ_f32_var, typ_type;
  typ_f64_var, typ_type;
  typ_bool_var, typ_type;
  typ_char_var, typ_type;
  typ_ptr_var, typ_pure_fun [typ_type] typ_type;
  typ_const_var, typ_pure_fun [typ_type] typ_type;
  typ_range_var, typ_type;
  Resource_formula.var_range, typ_pure_fun [typ_int; typ_int; typ_int] typ_range;
  Resource_formula.var_forall_in, typ_pure_fun [typ_range; typ_pure_fun [typ_int] typ_prop] typ_prop;
  Resource_formula.var_group, typ_pure_fun [typ_range; typ_pure_fun [typ_int] typ_hprop] typ_hprop;
  Resource_formula.var_frac, typ_type;
  Resource_formula.var_read_only, typ_pure_fun [Resource_formula.typ_frac; typ_hprop] typ_hprop;
  Resource_formula.var_uninit, typ_pure_fun [typ_hprop] typ_hprop;
  Resource_formula.var_is_true, typ_pure_fun [typ_bool] typ_prop;
  Resource_formula.var_is_false, typ_pure_fun [typ_bool] typ_prop;
  Resource_formula.var_not, typ_pure_fun [typ_prop] typ_prop;
  Resource_formula.var_and, typ_pure_fun [typ_prop; typ_prop] typ_prop;
  Resource_formula.var_or, typ_pure_fun [typ_prop; typ_prop] typ_prop;
  Resource_formula.var_in_range, typ_pure_fun [typ_int; typ_range] typ_prop;
  Resource_formula.var_is_subrange, typ_pure_fun [typ_range; typ_range] typ_prop;
  Resource_formula.var_range_count, typ_pure_fun [typ_range] typ_int;
  Resource_formula.var_wand, typ_pure_fun [typ_hprop; typ_hprop] typ_hprop;
  Resource_formula.var_frac_div, typ_pure_fun [typ_frac; typ_int] typ_frac;
  Resource_formula.var_frac_sub, typ_pure_fun [typ_frac; typ_frac] typ_frac;
  Resource_trm.var_ghost_ret, typ_type;
  Resource_trm.var_ghost_fn, typ_type; (* Maybe add an alias to trm_fun [] trm_ghost_ret *)
  var_ignore, typ_auto;
] ~fun_specs:(Var_map.add var_ignore ignore_spec mindex_specs) ()

(** [trm_recompute_resources t] recomputes resources of [t] using [compute_resources],
  after a [trm_deep_copy] to prevent sharing.
  Otherwise, returns a fresh term in case of success, or raises [ResourceError] in case of failure.

  If [!Flags.stop_on_first_resource_error] is set, then the function immediately stops after the
  first error is encountered, else it tries to report as many as possible.

  Hypothesis: needs var_ids to be calculated. *)
let trm_recompute_resources (t: trm): trm =
  (* TODO: should we avoid deep copy by maintaining invariant that there is no sharing?
     makes sense with unique var ids and trm_copy mechanisms.
     Otherwise avoid mutable asts. Could also have unique node ids and maps from ids to metadata. *)
  (* Make a copy of [t] *)
  let t = trm_deep_copy t in
  (* Typecheck the copy of [t] and update it in place.
     Type errors are saved in a global list, and errors
     are also saved as annotations in the "errors" fields
     in the nodes inside the copy of [t]. *)
  global_errors := [];
  let backtrace =
    try
      ignore (compute_resources (Some init_ctx) t);
      None
    with StoppedOnFirstError -> Some (Printexc.get_raw_backtrace ())
  in
  (* Test for errors, before returning the updated copy of [t] *)
  match !global_errors with
  | [] -> t
  | ((phase, exn) :: _) ->
      let err = ResourceError (t, phase, exn) in
      match backtrace with
      | Some bt -> Printexc.raise_with_backtrace err bt
      | None -> raise err
