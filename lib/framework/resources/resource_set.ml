open Ast
open Trm
open Resource_formula

(** A resource set contains pure and linear resource items, as well as aliases.

    Pure resource items are ordered (name, type) pairs, the name binds a pure variable in the rest of the resource set.
    e.g. [p: ptr, eq: 2 + 2 = 4, f: int -> formula]

    Pure resource items include function specifications. Function specifications are nameless and unordered between themselves but ordered after other pure resource items. They are stored in a map from function name to function specification for efficient lookup.

    Aliases map pure variables to their definition. Pure variables also include program variables (constant due to our AST encoding). Variables are subtituted by their definition whenever needed.

    Linear resource items are unordered (name, formula) pairs, the name binds a linear variable in the usage context. Two linear resources with the same name correspond to the same version, i.e. have the same value / model.
    e.g. [H1: p ~> Cell, H2: foreach i in 0..n -> &t[i] ~> Cell]
     *)

(** Makes a resource set given its components. *)
let make ?(pure = []) ?(linear = []) ?(fun_specs = Var_map.empty) ?(aliases = Var_map.empty) ?(struct_fields = Var_map.empty) () =
  { pure; linear; fun_specs; aliases; struct_fields }

(** The empty resource set. *)
let empty = empty_resource_set

let is_empty (res : resource_set) : bool =
  res.pure = [] && res.linear = [] && Var_map.is_empty res.fun_specs

(** If after consuming [old_res], [new_res] was produced, then [bind] generates the resulting resource set.
    Pure resources are accumulated, and linear resources are replaced.
*)
let bind ~(old_res: resource_set) ~(new_res: resource_set): resource_set =
  { pure = old_res.pure @ new_res.pure;
    linear = new_res.linear;
    fun_specs = Var_map.union (fun _ new_c _ -> Some new_c) new_res.fun_specs old_res.fun_specs;
    aliases = Var_map.union (fun _ new_d _ -> Some new_d) new_res.aliases old_res.aliases;
    struct_fields = Var_map.union (fun _ new_f _ -> Some new_f) new_res.struct_fields old_res.struct_fields; }

let find_pure (hyp : var) (res : resource_set) : formula option =
  List.find_map (fun (h, r) -> if var_eq h hyp then Some r else None) res.pure

let find_linear (hyp : var) (res : resource_set) : formula option =
  List.find_map (fun (h, r) -> if var_eq h hyp then Some r else None) res.linear

let find (hyp: var) (res: resource_set) : formula option =
  Option.or_else (find_pure hyp res) (fun () -> find_linear hyp res)

(** Returns the set of resource names bound by a resource set.*)
(* DEPRECATED
let resource_names (res: resource_set) : Var_set.t =
  let res_list_names (res: resource_item list) =
    List.fold_left (fun avoid_names (h, _) ->
            Var_set.add h avoid_names) Var_set.empty res
  in
  Var_set.union (res_list_names res.pure) (res_list_names res.linear)
*)

(** Pushes a pure resource item in front of a set. *)
let push_front_pure (res: resource_item) (res_set: resource_set) =
  { res_set with pure = res :: res_set.pure }

(** Pushes a pure resource item at the end of a set. *)
let push_back_pure (res: resource_item) (res_set: resource_set) =
  { res_set with pure = res_set.pure @ [res] }

(** Adds a linear resource item to a set. *)
let add_linear (res: resource_item) (res_set: resource_set) =
  { res_set with linear = res :: res_set.linear }

(** Adds multiple linear resource items to a set. *)
let add_linear_list (res: resource_item list) (res_set: resource_set) =
  { res_set with linear = res @ res_set.linear }

let add_alias (var: var) (alias: trm) (res_set: resource_set) =
  { res_set with aliases = Var_map.add var alias res_set.aliases }

let add_fun_spec (fun_var: var) (spec: fun_spec_resource) (res_set: resource_set) =
  { res_set with fun_specs = Var_map.add fun_var spec res_set.fun_specs }

let add_struct_fields (struct_name: var) (fields: (label * typ) list) (res_set: resource_set) =
  { res_set with struct_fields = Var_map.add struct_name fields res_set.struct_fields }

(** Given a resource set [res] that may depend on the [i] index of [range], produces the resource set resulting from grouping resource items forall [i]. *)
let group_range (range: loop_range) (res: resource_set): resource_set =
  { pure = List.map (fun (x, fi) -> (x, formula_forall_range range fi)) res.pure;
    linear = List.map (fun (x, fi) -> (x, formula_group_range range fi)) res.linear;
    fun_specs = res.fun_specs;
    aliases = res.aliases; (* FIXME: Probably wrong when aliasing variables from [pure] *)
    struct_fields = res.struct_fields; }

(** Given a resource set, produces a resource set with read-only access to its resources that can be duplicated: RW_in => RO_out; RO_out => RO_out * (trm_copy RO_out); *)
let read_only (res: resource_set): resource_set =
  let frac_var, frac_item = new_frac () in
  let frac = trm_var frac_var in
  let linear = List.map (fun (x, formula) ->
      let { formula } = formula_read_only_inv_all formula in
      (x, formula_read_only ~frac formula)
    ) res.linear in
  { res with pure = frac_item :: res.pure ; linear }

(** Returns the union of two resource sets, accumulating pure items and separating linear items with a star. *)
let union (res1: resource_set) (res2: resource_set): resource_set =
  { pure = res1.pure @ res2.pure; linear = res1.linear @ res2.linear;
    fun_specs = Var_map.union (fun _ _ c -> Some c) res1.fun_specs res2.fun_specs;
    aliases = Var_map.union (fun _ _ d -> Some d) res1.aliases res2.aliases;
    struct_fields = Var_map.union (fun _ _ f -> Some f) res1.struct_fields res2.struct_fields }

(** The built-in variable representing a function's return value. *)
(* FIXME: #var-id, id should be different for every let/letfun? like 'this' ids? *)
let var_result = toplevel_var "_Res"
let trm_result: formula = trm_var var_result

let map
  ?(f_pure : resource_item -> resource_item = fun r -> r)
  ?(f_linear : resource_item -> resource_item = fun r -> r)
  ?(f_alias : formula -> formula = fun f -> f)
  (res : resource_set) : resource_set =
  let pure = List.map f_pure res.pure in
  let linear = List.map f_linear res.linear in
  (* TODO: fun specs? *)
  let aliases = Var_map.map f_alias res.aliases in
  { pure; linear; fun_specs = res.fun_specs; aliases; struct_fields = res.struct_fields }

let filter_map
  ?(f_pure : resource_item -> resource_item option = fun r -> Some r)
  ?(f_linear : resource_item -> resource_item option = fun r -> Some r)
  ?(f_alias : var -> formula -> formula option = fun v f -> Some f)
  (res : resource_set) : resource_set =
  let pure = List.filter_map f_pure res.pure in
  let linear = List.filter_map f_linear res.linear in
  (* TODO: fun specs? *)
  let aliases = Var_map.filter_map f_alias res.aliases in
  { pure; linear; fun_specs = res.fun_specs; aliases; struct_fields = res.struct_fields }

(** Substitutes variables in a given resource set. *)
let rec subst (subst_map: tmap) (res: resource_set): resource_set =
  (* TODO: use map combinator? *)
  let subst_var_in_resource_list =
    List.map (fun (h, t) -> (h, trm_subst subst_map t))
  in
  let pure = subst_var_in_resource_list res.pure in
  let linear = subst_var_in_resource_list res.linear in
  let nores_subst_map = Var_map.remove var_result subst_map in
  let fun_specs =
    if Var_map.is_empty nores_subst_map then
      res.fun_specs
    else
      Var_map.map (fun spec ->
        { spec with contract = { pre = subst nores_subst_map spec.contract.pre; post = subst nores_subst_map spec.contract.post } })
        res.fun_specs
  in
  let aliases = Var_map.map (fun def -> trm_subst subst_map def) res.aliases in
  { pure; linear; fun_specs; aliases; struct_fields = res.struct_fields }

let subst_var (x: var) (t: trm) (res: resource_set) : resource_set =
  subst (Var_map.singleton x t) res

(** [rename_var x new_x res]: rename a variable [x] that should be bound in [res] as [new_x] *)
let rename_var (x: var) (new_x: var) (res: resource_set) : resource_set =
  let res = subst_var x (trm_var new_x) res in
  let rename_hyps_in_resource_list =
    List.map (fun (h, t) -> ((if var_eq h x then new_x else h), t))
  in
  let pure = rename_hyps_in_resource_list res.pure in
  let linear = rename_hyps_in_resource_list res.linear in
  let aliases = match Var_map.find_opt x res.aliases with
    | Some def -> res.aliases |> Var_map.remove x |> Var_map.add new_x def
    | None -> res.aliases
  in
  let fun_specs = match Var_map.find_opt x res.fun_specs with
  | None -> res.fun_specs
  | Some spec ->
    res.fun_specs |>
    Var_map.remove x |>
    Var_map.add new_x spec |>
    Var_map.map (fun spec_res -> {spec_res with inverse =
      Option.map (fun inv -> if var_eq inv x then new_x else inv) spec_res.inverse
    })
  in
  { pure; linear; aliases; fun_specs; struct_fields = res.struct_fields }

let subst_aliases (aliases: trm Var_map.t) (res: resource_set): resource_set =
  (* Invariant: [res.fun_specs] cannot refer variables in [res.aliases].
     This invariant is needed for performance reasons *)
  { (subst aliases { res with fun_specs = Var_map.empty }) with fun_specs = res.fun_specs }

let subst_all_aliases (res: resource_set): resource_set =
  let res' = subst_aliases res.aliases { res with aliases = Var_map.empty } in
  { res' with aliases = res.aliases }

exception Spec_not_found of var

let find_result_fun_spec (res: resource_set): fun_spec_resource =
  let fn_var = match Var_map.find_opt var_result res.aliases with
    | Some tf ->
      begin match trm_var_inv tf with
      | Some xf -> xf
      | None -> failwith "Weird alias (%s) found when searching for the specification of a function" (Ast_to_text.ast_to_string tf)
      end
    | None -> var_result
  in
  match Var_map.find_opt fn_var res.fun_specs with
    | Some spec -> spec
    | None -> raise (Spec_not_found fn_var)

(** Substitutes a loop index with its starting value. *)
let subst_loop_range_start range = subst_var range.index range.start

(** Substitutes a loop index with its value after one iteration *)
let subst_loop_range_step range = subst_var range.index (trm_add_int (trm_var range.index) range.step)

(** Substitutes a loop index with its end value. *)
let subst_loop_range_end range = subst_var range.index range.stop

(** [copy res]: refreshes all the binders in [res] to preserve var-id unicity. *)
let copy ?(subst = Var_map.empty) (res: resource_set): resource_set =
  match trm_fun_inv (trm_copy (trm_fun ~contract:(FunSpecContract { pre = res; post = empty }) [] Typ.typ_auto (trm_null Typ.typ_auto))) with
  | Some (_, _, _, FunSpecContract { pre }) -> pre
  | _ -> failwith "Resource_set.copy failed"


(** [bound_vars res] returns the set of pure variables that occur in [res] *)
let bound_vars (res: resource_set): Var_set.t =
  List.fold_left (fun set (x, _) -> Var_set.add x set) Var_set.empty res.pure

(** [used_vars res] returns the set of variables that are used inside [res].

    A variable is considered to be used if it is a free variable inside one formula in the resource set.
    Note that this list includes variables bound at the level of the resource set itself.
    This is useful for filtering unused variables in resource sets. *)
let used_vars (res: resource_set): Var_set.t =
  (* TODO: maybe check free vars inside function contracts? *)
  let combine_used_vars used_vars (_, formula) =
    Var_set.union used_vars (trm_free_vars formula)
  in
  let pure_used_vars = List.fold_left combine_used_vars Var_set.empty res.pure in
  List.fold_left combine_used_vars pure_used_vars res.linear

(** [remove_pure x res]: remove pure hypothesis x from res checking that it was not used *)
let remove_pure (x: var) (res: resource_set): resource_set =
  let used = used_vars res in
  if Var_set.mem x used then failwith "Cannot remove variable '%s' from context: it is used in another resource" (var_to_string x);
  { res with pure = List.filter (fun (y, _) -> not (var_eq x y)) res.pure; fun_specs = Var_map.remove x res.fun_specs; aliases = Var_map.remove x res.aliases }

(** [remove_useless_fracs usage res]: removes all fractions that have 0 occurences inside [res] from both [usage] and [res]. *)
(* LATER: Maybe we should have a more general remove_useless_inhabited_pure.*)
let remove_useless_fracs (usage: resource_usage_map) (res: resource_set): resource_usage_map * resource_set =
  let used = used_vars res in
  let usage = ref usage in
  let res = { res with
    pure = List.filter (fun (hyp, hyp_type) ->
      if (Trm_unify.are_same_trm hyp_type typ_frac) && not (Var_set.mem hyp used) then begin
        usage := Var_map.remove hyp !usage;
        false
      end else
        true
      ) res.pure
    } in
  !usage, res


(** [filter ?pure_filter ?linear_filter res] removes the resources for which the corresponding filter returns false.
    If a filter function is not given, do not filter the corresponding field. *)
let filter ?(pure_filter = fun _ -> true) ?(linear_filter = fun _ -> true) res =
  let fun_specs = ref res.fun_specs in
  let aliases = ref res.aliases in
  let pure = List.filter (fun (x, formula)  ->
    if pure_filter (x, formula) then true else begin
      fun_specs := Var_map.remove x !fun_specs;
      aliases := Var_map.remove x !aliases;
      false
    end) res.pure in
  let linear = List.filter linear_filter res.linear in
  { pure; linear; fun_specs = !fun_specs; aliases = !aliases; struct_fields = res.struct_fields }


type linear_usage_filter =
 { unused: bool; read_only: bool; joined_read_only: bool; uninit: bool; full: bool; produced: bool; }

let keep_all_linear = { unused = true; read_only = true; joined_read_only = true; uninit = true; full = true; produced = true; }
let keep_none_linear = { unused = false; read_only = false; joined_read_only = false; uninit = false; full = false; produced = false; }
let keep_touched_linear = { keep_all_linear with unused = false; }
let keep_used = { keep_touched_linear with produced = false; }
let keep_produced = { keep_none_linear with produced = true; }
let keep_unused = { keep_none_linear with unused = true; }
let keep_written = { keep_used with read_only = false; joined_read_only = false; }

(** A filter compatible with [List.filter] or [List.partition] that selects resources by their usage in the usage map given. *)
let linear_usage_filter usage filter (h, _) =
  match Var_map.find_opt h usage with
  | None -> filter.unused
  | Some SplittedFrac -> filter.read_only
  | Some JoinedFrac -> filter.joined_read_only
  | Some ConsumedUninit -> filter.uninit
  | Some ConsumedFull -> filter.full
  | Some Produced -> filter.produced
  | Some (Required | Ensured | ArbitrarilyChosen | Cleared) -> failwith "linear_usage_filter used on pure resource"

type pure_usage_filter =
 { unused: bool; required: bool; ensured: bool; arbitrarily_chosen: bool; cleared: bool }

let keep_all_pure = { unused = true; required = true; ensured = true; arbitrarily_chosen = true; cleared = true }
let keep_none_pure = { unused = false; required = false; ensured = false; arbitrarily_chosen = false; cleared = false }
let keep_touched_pure = { keep_all_pure with unused = false; }
let keep_unused_pure = { keep_none_pure with unused = true; }
let keep_required = { keep_none_pure with required = true; }
let keep_ensured = { keep_none_pure with ensured = true; }

(** A filter compatible with [List.filter] or [List.partition] that selects resources by their usage in the usage map given. *)
let pure_usage_filter usage filter (h, _) =
  match Var_map.find_opt h usage with
  | None -> filter.unused
  | Some Required -> filter.required
  | Some Ensured -> filter.ensured
  | Some ArbitrarilyChosen -> filter.arbitrarily_chosen
  | Some Cleared -> filter.cleared
  | Some (SplittedFrac | JoinedFrac | ConsumedUninit | ConsumedFull | Produced) ->
    failwith "pure_usage_filter used on linear resource"

let filter_touched (usage: resource_usage_map) (res: resource_set) =
  filter ~pure_filter:(pure_usage_filter usage keep_touched_pure) ~linear_filter:(linear_usage_filter usage keep_touched_linear) res

(** [filter_with_var x res]: keep only the resources of [res] that have an occurence of [x] as a free variable *)
let filter_with_var x res =
  let f (_, formula) = is_free_var_in_trm x formula in
  filter ~pure_filter:f ~linear_filter:f res
