open Ast
open Trm
open Typ
open Mark
open Resource_formula

(** A resource contract defines the effect of a loop or function on resources.
    User-facing contracts use convenient clauses (see {!contract_clause}) that are desugared into an internal contract representation ({!Ast.loop_contract}, {!Ast.fun_contract}).
    *)

(* TODO: module Resource_user / Resource_interface / Resource_syntax. *)

(** User-facing contract clause types.

  Giving a linear function contract clause to a for loop results in each iteration having separate resources.
  If consuming [R1] and producing [R2], the loop itself consumes [Group(range, R1)] and produces [Group(range, R2)].
  *)
type contract_clause_type =
  | Requires
  (** Pure resource items required in precondition.

      TODO: semantics for loops. *)

  | Ensures
  (** Pure resource items ensured in postcondition.

      TODO: semantics for loops. *)

  | Invariant
  (** Pure resource items that are invariant in all iterations of a for loop.
      If iterating on [i] index, requires [R(i)] and ensures [R(i + step)]. *)

  | Reads
  (** Syntactic sugar for consuming [_RO(f, R)] and producing [_RO(f, R)].
      [f] is a new fraction added to the required pure resource items.
      This is a function contract clause. *)

  | Writes
  (** Syntactic sugar for consuming [_Uninit(R)] and producing [R].
      This is a function contract clause. *)

  | Modifies
  (** Syntactic sugar for consuming [R] and producing [R].
      This is a function contract clause. *)

  | Consumes
  (** Linear resource items consumed in precondition.
      This is a function contract clause. *)

  | Produces
  (** Linear resource items produced in postcondition.
      This is a function contract clause. *)

  | SequentiallyReads
  (** Linear resource items that are passed as read only from any loop iteration to the next.
      Syntactic sugar for sequentially modifying [_RO(f, R)].
      [f] is a new fraction bound by the loop. *)

  | SequentiallyModifies
  (** Linear resource items that are invariantly modified in all loop iterations. *)

  | ParallelReads
  (** Linear resource items that are read in parallel by all loop iterations. *)

  | LoopGhosts
  (** Pure resources that are the same across all iterations. *)

(** User-facing contract clause, combining clause type and resouce item. *)
type contract_clause = contract_clause_type * contract_resource_item

(** The empty function contract, implies purity. *)
let empty_fun_contract =
  { pre = Resource_set.empty; post = Resource_set.empty }

(** {!trm_copy} for fun contracts. *)
let fun_contract_copy (contract : fun_contract) : fun_contract =
  let (_, _, _, spec) =
    trm_fun ~contract:(FunSpecContract contract) [] None (trm_seq_nomarks [])
    |> trm_copy
    |> trm_fun_inv |> Xoption.unsome
  in
  match spec with
  | FunSpecContract c' -> c'
  | _ -> failwith "should not happen"

(** The empty loop contract, implies purity. *)
let empty_loop_contract =
  { loop_ghosts = []; invariant = Resource_set.empty; parallel_reads = []; iter_contract = empty_fun_contract }

(* TODO: remove or replace with better mechanism. hint to maximize instantiated fraction. *)
let _Full = toplevel_var "_Full"

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
  let pre = Resource_set.add_linear (name, ro_formula) { contract.pre with pure = frac_ghost :: contract.pre.pure } in
  let post = Resource_set.add_linear (name, ro_formula) contract.post in
  { pre; post }

let resource_item_uninit ((name, formula): resource_item): resource_item =
  (name, formula_uninit formula)

(* LATER: Preserve user syntax using annotations *)
let push_fun_contract_clause (clause: contract_clause_type)
    (res: resource_item) (contract: fun_contract) =
  match clause with
  | Requires -> { contract with pre = Resource_set.push_front_pure res contract.pre }
  | Consumes -> { contract with pre = Resource_set.add_linear res contract.pre }
  | Ensures -> { contract with post = Resource_set.push_front_pure res contract.post }
  | Produces -> { contract with post = Resource_set.add_linear res contract.post }
  | Reads -> push_read_only_fun_contract_res res contract
  | Writes -> { pre = Resource_set.add_linear (resource_item_uninit res) contract.pre ; post = Resource_set.add_linear res contract.post }
  | Modifies -> { pre = Resource_set.add_linear res contract.pre ; post = Resource_set.add_linear res contract.post }
  | Invariant -> { pre = Resource_set.push_front_pure res contract.pre ; post = Resource_set.push_front_pure res contract.post }
  | SequentiallyReads -> failwith "SequentiallyReads only makes sense for loop contracts"
  | SequentiallyModifies -> failwith "SequentiallyModifies only makes sense for loop contracts"
  | ParallelReads -> failwith "ParallelReads only makes sense for loop contracts"
  | LoopGhosts -> failwith "LoopGhosts only makes sense for loop contracts"

let push_loop_contract_clause (clause: contract_clause_type)
    (res: resource_item) (contract: loop_contract) =
  match clause with
  | Invariant -> { contract with invariant = Resource_set.push_front_pure res contract.invariant }
  | Reads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; iter_contract = push_fun_contract_clause Modifies (name, ro_formula) contract.iter_contract }
  | SequentiallyReads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; invariant = Resource_set.add_linear (name, ro_formula) contract.invariant }
  | SequentiallyModifies ->
    { contract with invariant = Resource_set.add_linear res contract.invariant }
  | ParallelReads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; parallel_reads = (name, ro_formula) :: contract.parallel_reads }
  | LoopGhosts -> { contract with loop_ghosts = res :: contract.loop_ghosts }
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

(** [contract_outside_loop range contract] takes the [contract] of a for-loop over [range] and returns
  the contract of the for instruction. *)
let contract_outside_loop range contract =
  let invariant_before = Resource_set.subst_loop_range_start range contract.invariant in
  let pre = Resource_set.group_range range contract.iter_contract.pre in
  let pre = Resource_set.union invariant_before (Resource_set.add_linear_list contract.parallel_reads pre) in
  let pre = { pre with pure = contract.loop_ghosts @ pre.pure } in
  let invariant_after = Resource_set.subst_loop_range_end range contract.invariant in
  let post = Resource_set.group_range range contract.iter_contract.post in
  let post = Resource_set.add_linear_list contract.parallel_reads post in
  let post = Resource_set.union invariant_after post in
  { pre; post }

(** [contract_inside_loop range contract] takes the [contract] of a for-loop over [range] and returns
  the contract of its body. *)
let contract_inside_loop range contract =
  let pre = Resource_set.union contract.invariant (Resource_set.add_linear_list contract.parallel_reads contract.iter_contract.pre) in
  let pre = { pre with pure = contract.loop_ghosts @ pre.pure } in
  let invariant_after_one_iter = Resource_set.subst_loop_range_step range contract.invariant in
  let post = Resource_set.add_linear_list contract.parallel_reads contract.iter_contract.post in
  let post = Resource_set.union invariant_after_one_iter post in
  { pre; post }

(** [revert_fun_contract contract] returns a contract that swaps the resources produced and consumed. *)
let revert_fun_contract contract =
  assert (contract.post.pure = []);
  assert (contract.post.fun_specs = Var_map.empty);
  assert (contract.post.aliases = Var_map.empty);
  assert (contract.post.efracs = []);
  {
    pre = {
      pure = contract.pre.pure;
      linear = contract.post.linear;
      fun_specs = contract.pre.fun_specs;
      aliases = contract.pre.aliases;
      efracs = contract.pre.efracs;
    };
    post = {
      pure = [];
      linear = contract.pre.linear;
      fun_specs = Var_map.empty;
      aliases = Var_map.empty;
      efracs = [];
    }
  }

(** [fun_contract_subst ctx contract] substitutes variables from [ctx] inside [contract] *)
let fun_contract_subst ctx contract =
  { pre = Resource_set.subst ctx contract.pre;
    post = Resource_set.subst ctx contract.post }

(** [specialize_contract contract args] specializes the [contract] with the given [args], a subset of pure resources of the precondition *)
let specialize_contract contract args =
  fun_contract_subst args { contract with pre = { contract.pre with pure = List.filter (fun (ghost_var, _) -> not (Var_map.mem ghost_var args)) contract.pre.pure } }

(* TODO: rename and move elsewhere. *)
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

(** [fun_contract_used_vars res] returns the set of variables that are used inside [contract].

    A variable is considered to be used if it is a free variable inside one formula in the contract.
    Note that this list includes variables bound at the level of the contract itself.
    This is useful for filtering unused variables. *)
let fun_contract_used_vars contract =
  Var_set.union (Resource_set.used_vars contract.pre) (Resource_set.used_vars contract.post)

(** Same as [fun_contract_used_vars] for loop contracts *)
let loop_contract_used_vars contract =
  let combine_used_vars used_vars (_, formula) =
    Var_set.union used_vars (trm_free_vars formula)
  in
  List.fold_left combine_used_vars
    (List.fold_left combine_used_vars (Var_set.union (Resource_set.used_vars contract.invariant) (fun_contract_used_vars contract.iter_contract)) contract.parallel_reads)
    contract.loop_ghosts
