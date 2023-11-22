open Ast
open Trm
open Typ
open Mark
open Resource_formula

type contract_clause_type =
  | Requires
  | Ensures
  | Invariant
  | Reads
  | Writes
  | Modifies
  | Consumes
  | Produces
  | SequentiallyReads
  | SequentiallyModifies

type contract_clause = contract_clause_type * contract_resource_item

let resource_set ?(pure = []) ?(linear = []) ?(fun_specs = Var_map.empty) () =
  { pure; linear; fun_specs }

let empty_resource_set = resource_set ()

let empty_fun_contract =
  { pre = empty_resource_set; post = empty_resource_set }

let empty_loop_contract =
  { loop_ghosts = []; invariant = empty_resource_set; iter_contract = empty_fun_contract }


(* The built-in variable representing a function's return value. *)
(* FIXME: #var-id, id should change *)
let var_result = toplevel_var "_Res"
let trm_result: formula = trm_var var_result
let _Full = toplevel_var "_Full"
let __admitted = toplevel_var "__admitted"


let rec desugar_formula (formula: formula): formula =
  (* Warning: this function can be called on formulas with unresolved variable ids, therefore, we need to invert it by name *)
  (* With incremental var-id resolution we could heavily simplify this *)
  Pattern.pattern_match formula [
    Pattern.(trm_apps2 (trm_var (check (fun v -> v.name = "_HasModel"))) !__ (trm_apps (trm_var !__) !__ __)) (fun var f args ->
        if f.name <> sprintf "Matrix%d" (List.length args) then raise Pattern.Next;
        formula_matrix var args
      );
    Pattern.(!__) (fun formula -> trm_map desugar_formula formula)
  ]

let rec encode_formula (formula: formula): formula =
  match formula_matrix_inv formula with
  | Some (m, dims) -> formula_model m (trm_apps (trm_var (toplevel_var (sprintf "Matrix%d" (List.length dims)))) dims)
  | None -> trm_map encode_formula formula

let push_pure_res (res: resource_item) (res_set: resource_set) =
  { res_set with pure = res :: res_set.pure }

let push_linear_res (res: resource_item) (res_set: resource_set) =
  { res_set with linear = res :: res_set.linear }

let push_read_only_fun_contract_res ((name, formula): resource_item) (contract: fun_contract): fun_contract =
  let frac_var, frac_ghost = new_frac () in
  let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
  let pre = push_linear_res (name, ro_formula) { contract.pre with pure = frac_ghost :: contract.pre.pure } in
  let post = push_linear_res (name, ro_formula) contract.post in
  { pre; post }

let resource_item_uninit ((name, formula): resource_item): resource_item =
  (name, formula_uninit formula)

(* LATER: Preserve user syntax using annotations *)
let push_fun_contract_clause (clause: contract_clause_type)
    (res: resource_item) (contract: fun_contract) =
  match clause with
  | Requires -> { contract with pre = push_pure_res res contract.pre }
  | Consumes -> { contract with pre = push_linear_res res contract.pre }
  | Ensures -> { contract with post = push_pure_res res contract.post }
  | Produces -> { contract with post = push_linear_res res contract.post }
  | Reads -> push_read_only_fun_contract_res res contract
  | Writes -> { pre = push_linear_res (resource_item_uninit res) contract.pre ; post = push_linear_res res contract.post }
  | Modifies -> { pre = push_linear_res res contract.pre ; post = push_linear_res res contract.post }
  | Invariant -> { pre = push_pure_res res contract.pre ; post = push_pure_res res contract.post }
  | SequentiallyReads -> failwith "SequentiallyReads only makes sense for loop contracts"
  | SequentiallyModifies -> failwith "SequentiallyModifies only makes sense for loop contracts"

let push_loop_contract_clause (clause: contract_clause_type)
    (res: resource_item) (contract: loop_contract) =
  match clause with
  | Invariant -> { contract with invariant = push_pure_res res contract.invariant }
  | Reads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; iter_contract = push_fun_contract_clause Modifies (name, ro_formula) contract.iter_contract }
  | SequentiallyReads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; invariant = push_linear_res (name, ro_formula) contract.invariant }
  | SequentiallyModifies ->
    { contract with invariant = push_linear_res res contract.invariant }
  | _ -> { contract with iter_contract = push_fun_contract_clause clause res contract.iter_contract }

let parse_contract_res_item ((name, formula): contract_resource_item): resource_item =
  let name = match name with
    | Some h -> (* new_hyp_like *) h
    | None -> new_anon_hyp ()
  in
  (name, formula)

let parse_contract_clauses (empty_contract: 'c) (push_contract_clause: contract_clause_type -> resource_item -> 'c -> 'c) (clauses: (contract_clause_type * string) list) : 'c =
  List.fold_right (fun (clause, desc) contract  ->
      try
        let res_list = Resource_cparser.resource_list (Resource_clexer.lex_resources) (Lexing.from_string desc) in
        List.fold_right (fun res contract -> push_contract_clause clause (parse_contract_res_item res) contract) res_list contract
      with Resource_cparser.Error ->
        failwith ("Failed to parse resource: " ^ desc)
    ) clauses empty_contract

let res_group_range (range: loop_range) (res: resource_set): resource_set =
  { pure = List.map (fun (x, fi) -> (x, formula_group_range range fi)) res.pure;
    linear = List.map (fun (x, fi) -> (x, formula_group_range range fi)) res.linear;
    fun_specs = res.fun_specs; }

let res_union (res1: resource_set) (res2: resource_set): resource_set =
  { pure = res1.pure @ res2.pure; linear = res1.linear @ res2.linear;
    fun_specs = Var_map.union (fun _ _ c -> Some c) res1.fun_specs res2.fun_specs }

let rec subst_in_resources (subst_map: tmap) (res: resource_set): resource_set =
  let subst_var_in_resource_list =
    List.map (fun (h, t) -> (h, trm_subst subst_map t))
  in
  let pure = subst_var_in_resource_list res.pure in
  let linear = subst_var_in_resource_list res.linear in
  let nores_subst_map = Var_map.remove var_result subst_map in
  let fun_specs =
    Var_map.map (fun spec ->
      { spec with contract = { pre = subst_in_resources nores_subst_map spec.contract.pre; post = subst_in_resources nores_subst_map spec.contract.post } })
      res.fun_specs
  in
  { pure; linear; fun_specs }

let subst_var_in_resources (x: var) (t: trm) (res: resource_set) : resource_set =
  subst_in_resources (Var_map.singleton x t) res

let rename_var_in_resources (x: var) (new_x: var) (res: resource_set) : resource_set =
  let res = subst_var_in_resources x (trm_var new_x) res in
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

let subst_invariant_start (index, tstart, _, _, _, _) = subst_var_in_resources index tstart
let subst_invariant_step (index, _, _, _, step, _) = subst_var_in_resources index (trm_add (trm_var index) (loop_step_to_trm step))
let subst_invariant_end (index, _, _, tend, _, _) = subst_var_in_resources index tend

let loop_outer_contract range contract =
  let invariant_before = subst_invariant_start range contract.invariant in
  let pre = res_union invariant_before (res_group_range range contract.iter_contract.pre) in
  let pre = { pre with pure = contract.loop_ghosts @ pre.pure } in
  let invariant_after = subst_invariant_end range contract.invariant in
  let post = res_union invariant_after (res_group_range range contract.iter_contract.post) in
  { pre; post }

let loop_inner_contract range contract =
  let pre = res_union contract.invariant contract.iter_contract.pre in
  let pre = { pre with pure = contract.loop_ghosts @ pre.pure } in
  let invariant_after_one_iter = subst_invariant_step range contract.invariant in
  let post = res_union invariant_after_one_iter contract.iter_contract.post in
  { pre; post }

let revert_fun_contract contract =
  assert (contract.post.pure = []);
  assert (contract.post.fun_specs = Var_map.empty);
  {
    pre = {
      pure = contract.pre.pure;
      linear = contract.post.linear;
      fun_specs = contract.pre.fun_specs
    };
    post = {
      pure = [];
      linear = contract.pre.linear;
      fun_specs = Var_map.empty
    }
  }

let specialize_contract contract subst =
  { pre = subst_in_resources subst { contract.pre with pure = List.filter (fun (ghost_var, _) -> Var_map.mem ghost_var subst) contract.pre.pure };
    post = subst_in_resources subst contract.post }

let trm_specialized_ghost_closure ?(remove_contract = false) (ghost_call: trm) =
  Pattern.pattern_match ghost_call [
    Pattern.(trm_apps (trm_fun_with_contract nil !__ !__) nil !__) (fun ghost_body contract ghost_args ->
      let subst = List.fold_left (fun subst (g, t) -> Var_map.add g t subst) Var_map.empty ghost_args in
      let body = trm_subst subst ghost_body in
      let contract = if remove_contract then FunSpecUnknown else FunSpecContract (specialize_contract contract subst) in
      trm_fun [] None body ~contract
    );
    Pattern.(trm_apps !__ nil nil) (fun ghost_fn -> ghost_fn);
    Pattern.(trm_apps !__ nil !__) (fun ghost_fn ghost_args ->
      (* TODO: Handle this case by recovering the contract of the called function *)
      failwith "trm_specialized_ghost_closure: Unhandled complex ghost call that is not a closure"
    );
    Pattern.(!__) (fun _ -> failwith "trm_specialized_ghost_closure must be called with a ghost call as argument")
  ]
