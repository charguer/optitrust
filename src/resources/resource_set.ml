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

    TODO: should
    type hyp
    -->
    type pure_var
    type linear_var
    ?
     *)

(** Makes a resource set given its components. *)
let make ?(pure = []) ?(linear = []) ?(fun_specs = Var_map.empty) ?(aliases = Var_map.empty) ?(efracs = []) () =
  { pure; linear; fun_specs; aliases; efracs }

(** The empty resource set. *)
let empty = make ()

(** If after consuming [old_res], [new_res] was produced, then [bind] generates the resulting resource set.

    Pure resources are accumulated, and linear resources are replaced.
    *)
let bind ~(old_res: resource_set) ~(new_res: resource_set): resource_set =
  { pure = new_res.pure @ old_res.pure;
    linear = new_res.linear;
    fun_specs = Var_map.union (fun _ new_c _ -> Some new_c) new_res.fun_specs old_res.fun_specs;
    aliases = Var_map.union (fun _ new_d _ -> Some new_d) new_res.aliases old_res.aliases;
    efracs = new_res.efracs @ old_res.efracs; }

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

(** Adds a linear resource item to a set. *)
let add_linear (res: resource_item) (res_set: resource_set) =
  { res_set with linear = res :: res_set.linear }

(** Adds multiple linear resource items to a set. *)
let add_linear_list (res: resource_item list) (res_set: resource_set) =
  { res_set with linear = res @ res_set.linear }

(** Given a resource set [res] that may depend on the [i] index of [range], produces the resource set resulting from grouping resource items forall [i]. *)
let group_range (range: loop_range) (res: resource_set): resource_set =
  { pure = List.map (fun (x, fi) -> (x, formula_group_range range fi)) res.pure;
    linear = List.map (fun (x, fi) -> (x, formula_group_range range fi)) res.linear;
    fun_specs = res.fun_specs;
    aliases = res.aliases; (* FIXME: Probably wrong when aliasing variables from [pure] *)
    efracs = res.efracs; (* Maybe we loose in generality here *) }

(** Given a resource set, produces a resource set with read-only access to its resources that can be duplicated: RW_in => RO_out; RO_out => RO_out * (trm_copy RO_out); *)
let read_only (res: resource_set): resource_set =
  let frac_var, frac_item = new_frac () in
  let frac = trm_var frac_var in
  let linear = List.map (fun (x, formula) ->
      match formula_read_only_inv formula with
      (* makes the previous RO duplicable by not duplicating fractions. *)
      | Some { formula } -> (x, formula_read_only ~frac formula)
      | None -> (x, formula_read_only ~frac formula)) res.linear in
  { res with pure = frac_item :: res.pure ; linear; efracs = [] }

(** Returns the union of two resource sets, accumulating pure items and separating linear items with a star. *)
let union (res1: resource_set) (res2: resource_set): resource_set =
  { pure = res1.pure @ res2.pure; linear = res1.linear @ res2.linear;
    fun_specs = Var_map.union (fun _ _ c -> Some c) res1.fun_specs res2.fun_specs;
    aliases = Var_map.union (fun _ _ d -> Some d) res1.aliases res2.aliases;
    efracs = res1.efracs @ res2.efracs }

(** The built-in variable representing a function's return value. *)
(* FIXME: #var-id, id should be different for every let/letfun? like 'this' ids? *)
let var_result = toplevel_var "_Res"
let trm_result: formula = trm_var var_result


(** Substitutes variables in a given resource set. *)
let rec subst (subst_map: tmap) (res: resource_set): resource_set =
  let subst_var_in_resource_list =
    List.map (fun (h, t) -> (h, trm_subst subst_map t))
  in
  let pure = subst_var_in_resource_list res.pure in
  let linear = subst_var_in_resource_list res.linear in
  let nores_subst_map = Var_map.remove var_result subst_map in
  let fun_specs =
    Var_map.map (fun spec ->
      { spec with contract = { pre = subst nores_subst_map spec.contract.pre; post = subst nores_subst_map spec.contract.post } })
      res.fun_specs
  in
  let aliases = Var_map.map (fun def -> trm_subst subst_map def) res.aliases in
  let efracs = List.map (fun (f, lt_frac) -> (f, trm_subst subst_map lt_frac)) res.efracs in
  { pure; linear; fun_specs; aliases; efracs }

let subst_var (x: var) (t: trm) (res: resource_set) : resource_set =
  subst (Var_map.singleton x t) res

let rename_var (x: var) (new_x: var) (res: resource_set) : resource_set =
  (* FIXME: Decide what to do with subst when a variable is substituted by a non-variable in a variable-only context. *)
  let res = subst_var x (trm_var new_x) res in
  match Var_map.find_opt x res.fun_specs with
  | None -> res
  | Some spec ->
    { res with fun_specs = res.fun_specs |>
        Var_map.remove x |>
        Var_map.add new_x spec |>
        Var_map.map (fun spec_res -> {spec_res with inverse =
          Option.map (fun inv -> if var_eq inv x then new_x else inv) spec_res.inverse
        })
    }

let subst_all_aliases (res: resource_set): resource_set =
  subst res.aliases { res with aliases = Var_map.empty }

(** Substitutes a loop index with its starting value. *)
let subst_loop_range_start (index, tstart, _, _, _, _) = subst_var index tstart

(** Substitutes a loop index with its value after one iteration *)
let subst_loop_range_step (index, _, _, _, step, _) = subst_var index (trm_add (trm_var index) (Mark.loop_step_to_trm step))

(** Substitutes a loop index with its end value. *)
let subst_loop_range_end (index, _, _, tend, _, _) = subst_var index tend
