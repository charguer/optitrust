open Ast
open Trm
open Resource_formula

let make ?(pure = []) ?(linear = []) ?(fun_specs = Var_map.empty) () =
  { pure; linear; fun_specs }

let empty = make ()

let push_pure (res: resource_item) (res_set: resource_set) =
  { res_set with pure = res :: res_set.pure }

let push_linear (res: resource_item) (res_set: resource_set) =
  { res_set with linear = res :: res_set.linear }

let group_range (range: loop_range) (res: resource_set): resource_set =
  { pure = List.map (fun (x, fi) -> (x, formula_group_range range fi)) res.pure;
    linear = List.map (fun (x, fi) -> (x, formula_group_range range fi)) res.linear;
    fun_specs = res.fun_specs; }

let read_only (res: resource_set): resource_set =
  let frac_var, _ = new_frac () in
  let frac = trm_var frac_var in
  let linear = List.map (fun (x, formula) ->
      match formula_read_only_inv formula with
      | Some _ -> (x, formula)
      | None -> (x, formula_read_only ~frac formula)) res.linear in
  { res with linear }

let union (res1: resource_set) (res2: resource_set): resource_set =
  { pure = res1.pure @ res2.pure; linear = res1.linear @ res2.linear;
    fun_specs = Var_map.union (fun _ _ c -> Some c) res1.fun_specs res2.fun_specs }

(* The built-in variable representing a function's return value. *)
(* FIXME: #var-id, id should change *)
let var_result = toplevel_var "_Res"
let trm_result: formula = trm_var var_result

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
  { pure; linear; fun_specs }

let subst_var (x: var) (t: trm) (res: resource_set) : resource_set =
  subst (Var_map.singleton x t) res

let rename_var (x: var) (new_x: var) (res: resource_set) : resource_set =
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
