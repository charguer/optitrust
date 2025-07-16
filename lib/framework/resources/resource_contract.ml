open Ast
open Trm
open Typ
open Mark
open Resource_formula

(** A resource contract defines the effect of a loop or function on resources.
    User-facing contracts use convenient clauses (see {!contract_clause}) that are desugared into an internal contract representation ({!Ast.loop_contract}, {!Ast.fun_contract}).
    *)

(* TODO: module Resource_user / Resource_interface / Resource_syntax. *)

(** User-facing function contract clause types. *)
type fun_contract_clause_type =
  | Requires
  (** Pure resource items required in precondition. *)

  | Ensures
  (** Pure resource items ensured in postcondition. *)

  | Reads
  (** Syntactic sugar for consuming [_RO(f, R)] and producing [_RO(f, R)].
      [f] is a new fraction added to the required pure resource items. *)

  | Atomic
  (** Syntactic sugar for consuming [_ATOMIC(f, R)] and producing [_ATOMIC(f, R)].
      [f] is a new fraction added to the required pure resource items. *)

  | Writes
  (** Syntactic sugar for consuming [_Uninit(R)] and producing [R]. *)

  | Modifies
  (** Syntactic sugar for consuming [R] and producing [R] but with a different model. *)

  | Preserves
  (** Syntactic sugar for consuming [R] and producing [R] (same model). *)

  | Consumes
  (** Linear resource items consumed in precondition. *)

  | Produces
  (** Linear resource items produced in postcondition. *)

(** User-facing loop contract clause types. *)
type loop_contract_clause_type =
  | LoopGhosts
  (** Pure resources that are the same across all iterations. *)

  | Exclusive of fun_contract_clause_type
  (** Resources that are exclusive to one iteration. *)

  | InvariantGhosts
  (** Pure resource items that are invariant in all iterations of a for loop.
      If iterating on [i] index, requires [R(i)] and ensures [R(i + step)]. *)

  | SharedModifies
  (** Linear resource items that are invariantly modified in all loop iterations (same shape, different model). *)

  | SharedPreserves
  (** Linear resource items that are preserved across loop iterations but can be freely modified during the iteration. *)

  | SharedReads
  (** Linear resource items that are read in parallel by all loop iterations. *)

  | SharedAtomic
  (** Linear resource items that are written by atomic operations in parallel by all loop iterations. *)

  | Strict
  (** On loop contracts, do not allow other resources than the one mentionned inside the loop iterations.
      By default, all the resources that remain after instantiating the contract are considered as additional invariants.
      This permits the omission of most __smodifies and __sreads clauses. *)

(** {!trm_copy} for fun contracts. *)
let fun_contract_copy (contract : fun_contract) : fun_contract =
  let (_, _, _, spec) =
    trm_fun ~contract:(FunSpecContract contract) [] typ_auto (trm_seq_nomarks [])
    |> trm_copy
    |> trm_fun_inv |> Option.unsome
  in
  match spec with
  | FunSpecContract c' -> c'
  | _ -> failwith "should not happen"

(* TODO: remove or replace with better mechanism. hint to maximize instantiated fraction. *)
let _Full = toplevel_var "_Full"

let rec desugar_formula (formula: formula): formula =
  (* Warning: this function can be called on formulas with unresolved variable ids, therefore, we need to invert it by name *)
  (* With incremental var-id resolution we could heavily simplify this *)
  Pattern.pattern_match formula [
    Pattern.(trm_apps2 (trm_var_with_name var_repr.name) !__ (trm_apps (trm_var !__) !__ __ __)) (fun var f args () ->
        if !Flags.use_resources_with_models && f.name = sprintf "Matrix%d" (List.length args - 1) then
          let size, model = List.unlast args in
          formula_matrix var size ~model
        else if not (!Flags.use_resources_with_models) && f.name = sprintf "Matrix%d" (List.length args) then
          formula_matrix var args
        else if f.name = sprintf "UninitMatrix%d" (List.length args) then
          formula_uninit_matrix var args
        else raise Pattern.Next
      );
    (* Allow using operators / and - in first argument of RO(_,_) while normally they are reserved for integers *)
    Pattern.(trm_apps2 (trm_var_with_name var_read_only.name) !__ !__) (fun frac hprop () ->
        let rec desugar_frac f =
          Pattern.pattern_match f [
            Pattern.(trm_sub !__ !__) (fun frac1 frac2 () ->
              formula_frac_sub (desugar_frac frac1) (desugar_frac frac2));
            Pattern.(trm_trunc_div !__ !__) (fun base divisor () ->
              formula_frac_div (desugar_frac base) divisor);
            Pattern.(trm_int (eq 1)) (fun () -> full_frac);
            Pattern.__ (fun () -> f)
          ]
        in
        formula_read_only ~frac:(desugar_frac frac) (desugar_formula hprop)
      );
    Pattern.__ (fun () -> trm_map desugar_formula formula)
  ]

let rec encode_formula (formula: formula): formula =
  match formula_matrix_inv formula with
  | Some (m, dims, None) -> formula_repr m (trm_apps (trm_var (toplevel_var (sprintf "UninitMatrix%d" (List.length dims)))) dims)
  | Some (m, dims, Some model) ->
    if !Flags.use_resources_with_models then
      formula_repr m (trm_apps (trm_var (toplevel_var (sprintf "Matrix%d" (List.length dims)))) (dims @ [model]))
    else
      formula_repr m (trm_apps (trm_var (toplevel_var (sprintf "Matrix%d" (List.length dims)))) dims)
  | None -> trm_map encode_formula formula

(*ARTHUR : transformed "read_only" into "fractional_access"*)
(* Used both for read only and atomic operations. *)
let push_frac_access_fun_contract_res (formula_fun: frac:typ -> typ -> typ) ((name, formula): resource_item) (contract: fun_contract): fun_contract =
  let frac_var, frac_ghost = new_frac () in
  let frac_access_formula = formula_fun ~frac:(trm_var frac_var) formula in
  let pre = Resource_set.add_linear (name, frac_access_formula) { contract.pre with pure = frac_ghost :: contract.pre.pure } in
  let post = Resource_set.add_linear (name, frac_access_formula) contract.post in
  { pre; post }

let resource_item_uninit ((name, formula): resource_item): resource_item =
  (name, raw_formula_uninit formula)

(* LATER: Preserve user syntax using annotations *)
let push_fun_contract_clause (clause: fun_contract_clause_type)
    (res: resource_item) (contract: fun_contract) =
  match clause with
  | Requires -> { contract with pre = Resource_set.push_front_pure res contract.pre }
  | Consumes -> { contract with pre = Resource_set.add_linear res contract.pre }
  | Ensures -> { contract with post = Resource_set.push_front_pure res contract.post }
  | Produces -> { contract with post = Resource_set.add_linear res contract.post }
  | Reads -> push_frac_access_fun_contract_res formula_read_only res contract
  | Atomic -> push_frac_access_fun_contract_res formula_atomic res contract
  | Writes -> { pre = Resource_set.add_linear (resource_item_uninit res) contract.pre ; post = Resource_set.add_linear res contract.post }
  | Modifies | Preserves -> { pre = Resource_set.add_linear res contract.pre ; post = Resource_set.add_linear res contract.post }

(* ARTHUR: Copied code here, as no easy call on the corresponding function was possible. *)
let push_loop_contract_clause (clause: loop_contract_clause_type)
    (res: resource_item) (contract: loop_contract) =
  match clause with
  | LoopGhosts -> { contract with loop_ghosts = res :: contract.loop_ghosts }
  | Exclusive Reads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; iter_contract = push_fun_contract_clause Preserves (name, ro_formula) contract.iter_contract }
  | Exclusive clause -> { contract with iter_contract = push_fun_contract_clause clause res contract.iter_contract }
  | InvariantGhosts -> { contract with invariant = Resource_set.push_front_pure res contract.invariant }
  | SharedModifies | SharedPreserves ->
    { contract with invariant = Resource_set.add_linear res contract.invariant }
  | SharedReads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; parallel_reads = (name, ro_formula) :: contract.parallel_reads }
  | SharedAtomic ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let atomic_formula = formula_atomic ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; parallel_atomic = (name, atomic_formula) :: contract.parallel_atomic }
  | Strict -> failwith "Strict should never appear with resources inside"

let parse_contract_res_item ((name, formula): contract_resource_item): resource_item =
  let name = match name with
    | Some h -> h
    | None -> new_anon_hyp ()
  in
  (name, formula)

let parse_contract_clauses (empty_contract: 'c) (push_contract_clause: 'clause_type -> resource_item -> 'c -> 'c) (clauses: ('clause_type * string) list) : 'c =
  List.fold_right (fun (clause, desc) contract  ->
      try
        let res_list = Resource_cparser.resource_list (Resource_clexer.lex_resources) (Lexing.from_string desc) in
        List.fold_right (fun res contract -> push_contract_clause clause (parse_contract_res_item res) contract) res_list contract
      with
      | Resource_cparser.Error -> failwith "Failed to parse resource: %s" desc
      | Resource_clexer.SyntaxError err -> failwith "Failed to lex resources '%s' : %s" desc err
    ) clauses empty_contract

(** [contract_outside_loop range contract] takes the [contract] of a for-loop over [range] and returns
  the contract of the for instruction. *)
let contract_outside_loop range contract =
  let invariant_before = Resource_set.subst_loop_range_start range contract.invariant in
  let pre = Resource_set.group_range range contract.iter_contract.pre in
  let pre = Resource_set.union invariant_before (Resource_set.add_linear_list (contract.parallel_reads @ contract.parallel_atomic) pre) in
  let pre = { pre with pure = contract.loop_ghosts @ pre.pure } in
  let invariant_after = Resource_set.subst_loop_range_end range contract.invariant in
  let post = Resource_set.group_range range contract.iter_contract.post in
  let post = Resource_set.add_linear_list (contract.parallel_reads @ contract.parallel_atomic) post in
  let post = Resource_set.union invariant_after post in
  { pre; post }

let parallel_reads_inside_loop range par_reads =
  List.map (fun (x, formula) ->
      let { frac; formula } = formula_read_only_inv_all formula in
      (x, formula_read_only ~frac:(formula_frac_div frac (formula_range_count (formula_loop_range range))) formula)
    ) par_reads

let parallel_atomic_inside_loop range par_reads =
  List.map (fun (x, formula) ->
      let { frac; formula } = formula_atomic_inv_all formula in
      (x, formula_atomic ~frac:(formula_frac_div frac (formula_range_count (formula_loop_range range))) formula)
    ) par_reads

(** [contract_inside_loop range contract] takes the [contract] of a for-loop over [range] and returns
  the contract of its body. *)
let contract_inside_loop range contract =
  let par_atomic_inside = parallel_atomic_inside_loop range contract.parallel_atomic in
  let par_reads_inside = parallel_reads_inside_loop range contract.parallel_reads in
  let pre = Resource_set.union contract.invariant (Resource_set.add_linear_list (par_reads_inside @ par_atomic_inside) contract.iter_contract.pre) in
  let index_in_range_hyp = (new_anon_hyp (), formula_in_range (trm_var range.index) (formula_loop_range range)) in
  let pre = { pre with pure = (range.index, typ_int) :: index_in_range_hyp :: contract.loop_ghosts @ pre.pure } in
  let invariant_after_one_iter = Resource_set.subst_loop_range_step range contract.invariant in
  let post = Resource_set.add_linear_list (par_reads_inside @ par_atomic_inside) contract.iter_contract.post in
  let post = Resource_set.union invariant_after_one_iter post in
  { pre; post }

(** [revert_fun_contract contract] returns a contract that swaps the resources produced and consumed. *)
let revert_fun_contract contract =
  assert (contract.post.fun_specs = Var_map.empty);
  assert (contract.post.aliases = Var_map.empty);
  {
    pre = {
      pure = contract.pre.pure @ contract.post.pure;
      linear = contract.post.linear;
      fun_specs = contract.pre.fun_specs;
      aliases = contract.pre.aliases;
      struct_fields = contract.pre.struct_fields;
    };
    post = {
      pure = [];
      linear = contract.pre.linear;
      fun_specs = Var_map.empty;
      aliases = Var_map.empty;
      struct_fields = Var_map.empty;
    }
  }

(** [fun_contract_subst ctx contract] substitutes variables from [ctx] inside [contract] *)
let fun_contract_subst ctx contract =
  { pre = Resource_set.subst ctx contract.pre;
    post = Resource_set.subst ctx contract.post }

(* ARTHUR: no change, is ok ?*)
(** [loop_contract_subst ctx contract] substitutes variables from [ctx] inside [contract] *)
let loop_contract_subst ctx contract =
  { loop_ghosts = contract.loop_ghosts;
    invariant = Resource_set.subst ctx contract.invariant;
    parallel_reads = List.map (fun (h, t) -> (h, trm_subst ctx t)) contract.parallel_reads;
    parallel_atomic = List.map (fun (h, t) -> (h, trm_subst ctx t)) contract.parallel_atomic;
    iter_contract = fun_contract_subst ctx contract.iter_contract;
    strict = contract.strict }

(** [specialize_contract contract args] specializes the [contract] with the given [args], a subset of pure resources of the precondition *)
let specialize_contract contract args =
  fun_contract_subst args { contract with pre = { contract.pre with pure = List.filter (fun (ghost_var, _) -> not (Var_map.mem ghost_var args)) contract.pre.pure } }

(* TODO: rename and move elsewhere. *)
let trm_specialized_ghost_closure ?(remove_contract = false) (ghost_call: trm) =
  Pattern.pattern_match ghost_call [
    Pattern.(trm_apps (trm_fun_with_contract nil !__ !__) nil !__ __) (fun ghost_body contract ghost_args () ->
      let subst = List.fold_left (fun subst (g, t) -> Var_map.add g t subst) Var_map.empty ghost_args in
      let body = trm_subst subst ghost_body in
      let contract = if remove_contract then FunSpecUnknown else FunSpecContract (specialize_contract contract subst) in
      trm_fun [] typ_auto body ~contract
    );
    Pattern.(trm_apps !__ nil nil nil) (fun ghost_fn () -> ghost_fn);
    Pattern.(trm_apps !__ nil !__ !__) (fun ghost_fn ghost_args ghost_bind () ->
      (* TODO: Handle this case by recovering the contract of the called function *)
      failwith "trm_specialized_ghost_closure: Unhandled complex ghost call that is not a closure"
    );
    Pattern.__ (fun () -> failwith "trm_specialized_ghost_closure must be called with a ghost call as argument")
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
