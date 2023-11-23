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


let empty_fun_contract =
  { pre = Resource_set.empty; post = Resource_set.empty }

let empty_loop_contract =
  { loop_ghosts = []; invariant = Resource_set.empty; iter_contract = empty_fun_contract }


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

let push_read_only_fun_contract_res ((name, formula): resource_item) (contract: fun_contract): fun_contract =
  let frac_var, frac_ghost = new_frac () in
  let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
  let pre = Resource_set.push_linear (name, ro_formula) { contract.pre with pure = frac_ghost :: contract.pre.pure } in
  let post = Resource_set.push_linear (name, ro_formula) contract.post in
  { pre; post }

let resource_item_uninit ((name, formula): resource_item): resource_item =
  (name, formula_uninit formula)

(* LATER: Preserve user syntax using annotations *)
let push_fun_contract_clause (clause: contract_clause_type)
    (res: resource_item) (contract: fun_contract) =
  match clause with
  | Requires -> { contract with pre = Resource_set.push_pure res contract.pre }
  | Consumes -> { contract with pre = Resource_set.push_linear res contract.pre }
  | Ensures -> { contract with post = Resource_set.push_pure res contract.post }
  | Produces -> { contract with post = Resource_set.push_linear res contract.post }
  | Reads -> push_read_only_fun_contract_res res contract
  | Writes -> { pre = Resource_set.push_linear (resource_item_uninit res) contract.pre ; post = Resource_set.push_linear res contract.post }
  | Modifies -> { pre = Resource_set.push_linear res contract.pre ; post = Resource_set.push_linear res contract.post }
  | Invariant -> { pre = Resource_set.push_pure res contract.pre ; post = Resource_set.push_pure res contract.post }
  | SequentiallyReads -> failwith "SequentiallyReads only makes sense for loop contracts"
  | SequentiallyModifies -> failwith "SequentiallyModifies only makes sense for loop contracts"

let push_loop_contract_clause (clause: contract_clause_type)
    (res: resource_item) (contract: loop_contract) =
  match clause with
  | Invariant -> { contract with invariant = Resource_set.push_pure res contract.invariant }
  | Reads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; iter_contract = push_fun_contract_clause Modifies (name, ro_formula) contract.iter_contract }
  | SequentiallyReads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; invariant = Resource_set.push_linear (name, ro_formula) contract.invariant }
  | SequentiallyModifies ->
    { contract with invariant = Resource_set.push_linear res contract.invariant }
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

let subst_invariant_start (index, tstart, _, _, _, _) = Resource_set.subst_var index tstart
let subst_invariant_step (index, _, _, _, step, _) = Resource_set.subst_var index (trm_add (trm_var index) (loop_step_to_trm step))
let subst_invariant_end (index, _, _, tend, _, _) = Resource_set.subst_var index tend

let loop_outer_contract range contract =
  let invariant_before = subst_invariant_start range contract.invariant in
  let pre = Resource_set.union invariant_before (Resource_set.group_range range contract.iter_contract.pre) in
  let pre = { pre with pure = contract.loop_ghosts @ pre.pure } in
  let invariant_after = subst_invariant_end range contract.invariant in
  let post = Resource_set.union invariant_after (Resource_set.group_range range contract.iter_contract.post) in
  { pre; post }

let loop_inner_contract range contract =
  let pre = Resource_set.union contract.invariant contract.iter_contract.pre in
  let pre = { pre with pure = contract.loop_ghosts @ pre.pure } in
  let invariant_after_one_iter = subst_invariant_step range contract.invariant in
  let post = Resource_set.union invariant_after_one_iter contract.iter_contract.post in
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
  { pre = Resource_set.subst subst { contract.pre with pure = List.filter (fun (ghost_var, _) -> Var_map.mem ghost_var subst) contract.pre.pure };
    post = Resource_set.subst subst contract.post }

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

let fun_contract_free_vars (contract: fun_contract): Var_set.t =
  let fold_res_list bound_vars fv res =
    List.fold_left (fun (bound_vars, fv) (h, formula) ->
      let bound_vars = Var_set.add h bound_vars in
      (bound_vars, Var_set.union (trm_free_vars ~bound_vars formula) fv)) (bound_vars, fv) res
  in
  let bound_vars, free_vars = fold_res_list Var_set.empty Var_set.empty contract.pre.pure in
  let _, free_vars = fold_res_list bound_vars free_vars contract.pre.linear in
  let bound_vars, free_vars = fold_res_list bound_vars free_vars contract.post.pure in
  let _, free_vars = fold_res_list bound_vars free_vars contract.post.linear in
  free_vars
