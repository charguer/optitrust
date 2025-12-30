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
          formula_matrix var ~mem_typ:(mem_typ_any) size ~model
        else if !Flags.use_resources_with_models && f.name = sprintf "MatrixOf%d" (List.length args - 2) then (* TODO: only bothering with models mode for now, seems like there is a better way to organize this though *)
          let args, model = List.unlast args in
          let size, mem = List.unlast args in
          formula_matrix var ~mem_typ:(mem) size ~model
        else if not (!Flags.use_resources_with_models) && f.name = sprintf "Matrix%d" (List.length args) then
          formula_matrix var ~mem_typ:(mem_typ_any) args
        else if f.name = sprintf "UninitMatrix%d" (List.length args) then
          formula_uninit_matrix ~mem_typ:(mem_typ_any) var args
        else if !Flags.use_resources_with_models && f.name = sprintf "UninitMatrixOf%d" (List.length args - 1) then
          let size, mem = List.unlast args in
          formula_uninit_matrix ~mem_typ:(mem) var size
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
  | Some (m, dims, Some (model,_)) -> (* TODO: handle other hardware types *)
    if !Flags.use_resources_with_models then
      formula_repr m (trm_apps (trm_var (toplevel_var (sprintf "Matrix%d" (List.length dims)))) (dims @ [model]))
    else
      formula_repr m (trm_apps (trm_var (toplevel_var (sprintf "Matrix%d" (List.length dims)))) dims)
  | None -> trm_map encode_formula formula

let push_read_only_fun_contract_res ((name, formula): resource_item) (contract: fun_contract): fun_contract =
  let frac_var, frac_ghost = new_frac () in
  let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
  let pre = Resource_set.add_linear (name, ro_formula) { contract.pre with pure = frac_ghost :: contract.pre.pure } in
  let post = Resource_set.add_linear (name, ro_formula) contract.post in
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
  | Reads -> push_read_only_fun_contract_res res contract
  | Writes -> { pre = Resource_set.add_linear (resource_item_uninit res) contract.pre ; post = Resource_set.add_linear res contract.post }
  | Modifies | Preserves -> { pre = Resource_set.add_linear res contract.pre ; post = Resource_set.add_linear res contract.post }

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

(* Thread for info data structure: Need to know:
  - Rin: trm (a range thing)
  - Rout: trm (the range inside the loop, which is R_out(i) in the typing rule,
   but note we do NOT use the trm_apps inside the contract! we dont want to have to beta-reduce this (we CANT do this at unification in general, ok well we can since its nonterminating but its arbitrary complexity - dependent types stuff).)
   it can also be var -> trm if the index var is not avail. in the thread for extract fn for some reason
  - M: the bounds of the thread for loop

  NOTE: THESE NOMENCLATURES ARE CONFUSING: SWAP RIN and ROUT because the sense i mean it here is input and output,
    when it should be inner and outer to be consistent, so it should be swapped
*)

(* Steps to extract the above data structure from the thread for.
1. Check the well-formedness of thread for, based on loop_range data structure. If everything is good, give R.end (=M in on-paper typing rule). Return option trm
2. Check that Rin is well-formed (this I think is technically a typechecking step but not sure? idk)
  For this we have to look for a ThreadsCtx in the context. If we find none, or more than one, we have not succesfully typechecked
  (TODO: Is this bad?? This seems like it's duplicating the effort of the normal contract checking, but maybe that's ok because it just means it will be safer (two extra checks)??)
  Then we match on the structure of the range inside the threadsctx according to the rules I've put in the on-paper rule.
  We extract:
  - an upper list of dimensions DN+K-1..DK+1, and a lower list DK-1..D1 (M=DK is sandwiched between)
  - a list of indices tN..t2
  - the arity N (but it's inferrable from the first two lists so idk)
  This stays in the function. It does not go in the data structure above.
  If all of this pattern matching passes, we save Rin to the thread for info data structure.
  Note we don't save ThreadsCtx(Rin) currently, so technically we have to reconstruct this term in the outer pre and post
  we could store it instead but yeah who knows.
3. Create the inner range term R_out (kind of confusing once again see above).
  Pretty easy to just construct the MINDEX, MSIZE, RangePlus terms based on the above lists we extracted.

Note the extraction runs at the very start of compute_resources (or maybe after doing pure typechecking of inner contract?).
*)

(** [contract_outside_loop range contract] takes the [contract] of a for-loop over [range] and returns
  the contract of the for instruction. *)
let contract_outside_loop threadfor_info range contract =
  (* First Check that contract.invariant is empty for thread for *)
  (* Pre: better have
      - Rin = tin..+N with very specific requirements with MINDEX, MSIZE, etc. => COMPUTE THIS INFO FIRST AND MAKE A STRUCT
      - ThreadsCtx(Rin)
      - DesyncGroup of all of our xpre with Rin and 0..loop range
      - Check the loop range somehow (but this is not a contract or types thing just a thread for well formedness moreso) => GOES IN THE SAME HELPER AS #1 (extract_thread_for_info)
      - *)
  (* Post: produce
      - Rin : same
      - ThreadsCtx(Rin)
      - DesyncGroup with same Rin and range
      *)
  let grp_apply_fn = match threadfor_info with
    | Some info ->
      (* TODO: should the pre & post share the threadsctx variable or no? *)
      let tctx = (new_anon_hyp (),formula_threadsctx info.r_out) in
      fun range res -> (
        let res = Resource_set.desyncgroup_range info.r_out range res in
        { res with linear = tctx :: res.linear }
      )
    | _ ->  Resource_set.group_range in
  let invariant_before = Resource_set.subst_loop_range_start range contract.invariant in
  let pre = grp_apply_fn range contract.iter_contract.pre in
  let pre = Resource_set.union invariant_before (Resource_set.add_linear_list contract.parallel_reads pre) in
  let pre = { pre with pure = contract.loop_ghosts @ pre.pure } in
  let invariant_after = Resource_set.subst_loop_range_end range contract.invariant in
  let post = grp_apply_fn range contract.iter_contract.post in
  let post = Resource_set.add_linear_list contract.parallel_reads post in
  let post = Resource_set.union invariant_after post in
  { pre; post }

let parallel_reads_inside_loop range par_reads =
  List.map (fun (x, formula) ->
      let { frac; formula } = formula_read_only_inv_all formula in
      (x, formula_read_only ~frac:(formula_frac_div frac (formula_range_count (formula_loop_range range))) formula)
    ) par_reads

(** [contract_inside_loop range contract] takes the [contract] of a for-loop over [range] and returns
  the contract of its body. *)
let contract_inside_loop threadfor_info range contract =
  (*  (Optionally check emptyness of contract here as well for thread for)
  (assume same loop range requirements) Pre: give
    - i in range 0..N
    - ThreadsCtx(R_out(i)) (calculate R_out from R_in etc.)
    *)
  (* Post: expect
    - i in range 0..N
    - ThreadsCtx(R_out(i)) (preserved)*)
  let par_reads_inside = parallel_reads_inside_loop range contract.parallel_reads in
  let pre = Resource_set.union contract.invariant (Resource_set.add_linear_list par_reads_inside contract.iter_contract.pre) in
  let index_in_range_hyp = (new_anon_hyp (), formula_in_range (trm_var range.index) (formula_loop_range range)) in
  let pre = { pre with pure = (range.index, typ_int) :: index_in_range_hyp :: contract.loop_ghosts @ pre.pure } in
  let invariant_after_one_iter = Resource_set.subst_loop_range_step range contract.invariant in
  let post = Resource_set.add_linear_list par_reads_inside contract.iter_contract.post in
  let post = Resource_set.union invariant_after_one_iter post in
  match threadfor_info with
  | Some info ->
    if not (Resource_set.is_empty contract.invariant) then failwith "Invariant not allowed inside threadfor!" else ();
    (* TODO: should the pre & post share the threadsctx variable or no? *)
    let tctx = (new_anon_hyp (),formula_threadsctx info.r_in) in
    let add_tctx res = { res with linear = tctx :: res.linear } in
    { pre = add_tctx pre; post = add_tctx post }
  | _ -> { pre; post }

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

(** [loop_contract_subst ctx contract] substitutes variables from [ctx] inside [contract] *)
let loop_contract_subst ctx contract =
  { loop_ghosts = contract.loop_ghosts;
    invariant = Resource_set.subst ctx contract.invariant;
    parallel_reads = List.map (fun (h, t) -> (h, trm_subst ctx t)) contract.parallel_reads;
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
