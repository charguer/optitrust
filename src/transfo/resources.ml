open Prelude
open Target
open Resource_formula
open Resource_contract

let ensure_computed = Trace.recompute_resources

(* TODO: avoid recomputing all resources for validity checks. *)
let required_for_check () : unit =
  if !Flags.check_validity then ensure_computed ()

let justif_correct (why : string) : unit =
  if !Flags.check_validity then begin
    ensure_computed ();
    Trace.justif (sprintf "resources are correct: %s" why)
  end


(** Returns the resource usage of the given term, fails if unavailable. *)
let usage_of_trm (t : trm) =
  unsome_or_trm_fail t "expected resource usage to be available" t.ctx.ctx_resources_usage

(** Returns the resources available before the given term,
    fails if unavailable. *)
let before_trm (t : trm) =
  unsome_or_trm_fail t "expected resources before to be available" t.ctx.ctx_resources_before

(** Returns the resources available before the given term,
    fails if unavailable. *)
let after_trm (t : trm) =
  unsome_or_trm_fail t "expected resources after to be available" t.ctx.ctx_resources_after

(** Returns the instantiation of the post condition perfomed after the term t,
    fails if unavailable. *)
let post_inst (t: trm) =
  unsome_or_trm_fail t "expected instantiation of a post-condition to be available" t.ctx.ctx_resources_post_inst

(** Computes the resource usage of a consecutive sequence of instructions. *)
let compute_usage_of_instrs (instrs : trm mlist) : resource_usage_map =
  List.fold_left (fun usage_map t ->
    Resource_computation.update_usage_map ~current_usage:usage_map ~extra_usage:(usage_of_trm t)
  ) Resource_computation.empty_usage_map (Mlist.to_list instrs)

(** [trm_is_pure]: Does this term always evaluate to the same value?
    The computed value should no be affected by observable program state.
    Computing the value should not affect the observable program state. *)
let trm_is_pure (t: trm): bool =
  (* TODO: ? look at resource usage, empty linear usage is pure *)
  Option.is_some (Resource_formula.formula_of_trm t)

(** If output_new_fracs is given, do not add new fractions to the pure precondition but add them to the list instead. *)
let minimize_fun_contract ?(output_new_fracs: resource_item list ref option) (contract: fun_contract) (post_inst: used_resource_set) (usage: resource_usage_map): fun_contract =
  let open Resource_computation in
  let post_renaming, post_renaming_back = List.fold_left (fun (renaming, renaming_back) { hyp; inst_by } ->
      let inst_by_hyp = Formula_inst.origin_hyp inst_by in
      (Var_map.add hyp inst_by_hyp renaming, Var_map.add inst_by_hyp hyp renaming_back)
    ) (Var_map.empty, Var_map.empty) post_inst.used_linear
  in
  let renamed_post = List.map (fun (hyp, formula) -> (Var_map.find hyp post_renaming, formula)) contract.post.linear in
  let { new_fracs; linear_pre; linear_post } = minimize_linear_triple contract.pre.linear renamed_post usage in
  let linear_post = List.map (fun (hyp, formula) -> (Option.value ~default:hyp (Var_map.find_opt hyp post_renaming_back), formula)) linear_post in

  let added_fracs = match output_new_fracs with
  | Some out ->
    out := new_fracs @ !out;
    []
  | None -> new_fracs
  in

  let new_contract = {
    pre = { contract.pre with pure = added_fracs @ contract.pre.pure ; linear = linear_pre };
    post = { contract.post with linear = linear_post }
  } in
  let new_contract_used_vars = fun_contract_used_vars new_contract in

  let filter_pure_pre (x, formula) =
    Var_set.mem x new_contract_used_vars || (not (are_same_trm formula trm_frac) && Var_map.mem x usage)
  in

  { new_contract with
    pre = { new_contract.pre with
      pure = List.filter filter_pure_pre new_contract.pre.pure
    }
  }

let fun_minimize_on (t: trm): trm =
  let name, typ, args, body, contract = match t.desc with
    | Trm_let_fun (name, typ, args, body, FunSpecContract contract) ->
        name, typ, args, body, contract
    | _ -> failwith "fun_minimize_on: not a function definition with a contract"
  in

  let body_usage = usage_of_trm body in
  let post_inst = post_inst body in
  let new_contract = minimize_fun_contract contract post_inst body_usage in
  trm_like ~old:t (trm_let_fun name typ args body ~contract:(FunSpecContract new_contract))

(* [fun_minimize]: minimize a function contract by looking at the resource usage of its body *)
let%transfo fun_minimize (tg: target) : unit =
  ensure_computed ();
  Target.apply_at_target_paths fun_minimize_on tg;
  justif_correct "only changed function contracts"


(** Specification of loop minimization.

    When using OptiTrust resource system, all loops are considered to
    have a contract. It can be either specified by annotations or
    it can be implicitly equal to the default contract.
    The default contract is made of an invariant that contains all the
    available resources before the loop.
    Both default and annotated contracts can be minimized using the
    loop-contract minimization procedure described below. This procedures
    adds or modifies the contract annotations of the loop it operates on.
    Therefore, it is useful to clean up unused resources.

    The minimization operation assumes a well-typed code. It focuses on a loop,
    and attempts to replace the loop contract with another simpler contract,
    in the sense that it has a smaller (or similar) footprint.
    The output code is guaranteed to typecheck with the new contracts.
    (LATER: If the contract is not modified, the term is not modified.)

    The minimization operation works as follows;
    - we look at the usage of each of the resources, which was computed
      during the typechecking;
    - if a resource is not used at all, or used in a weaker mode
      than what is available, the contracts are refined.

    The possible cases are as follows:

    (1) If the resource comes from the invariant:
      - if it is never used, it is removed (from the invariant)
      - if it is available in RW but only used in SplitRO,
        then it is replaced with RO
        (internally, a fresh fraction is introduced for that RO)
      - if it is available in RW but only used as Uninit (see counter-example below);
        the user can always change the contract manually.

    (2) If the resource comes from the consumes clause:
      - if it is never used (not in the usage map),
        it must be the case that this resource appears in the produces clause;
        it this case, the resource is removed from both consumes and produces
      - if if is used in Full mode, there is nothing to simplify
      - if the resource appears in RW in consumes AND in produces clauses (potentially as Uninit),
        and it is used only in RO (usage SplittedFrac), then it may
        be replaced by a RO resource both in consumes and produces clauses
        (LATER: actually we could also change the fraction of RO resources always
        used in SplitRO mode, to remove same fraction constraints)
      - if the resource is consumed in RW and used in Uninit, then the RW
        may be replaced by an Uninit resource in the consumes clause.


    (TODO: Document function contract minimization separately, and use it to
    describe iter_contract minimization)

    Note: it may be tempting to say that a RW resource used in Uninit can
    be weakened in the post-condition, however it depends on the context
    whether it is feasible: it is feasible only if the operations after
    the loop require Uninit and not RW. Example:
      t = 1
      for i // RW(t) cannot be replaced by Uninit(t)
        t = 2
        x += t
      x += t // this read can be done with RW(t) but not with Uninit(t)
    Note, however, that if the last line was a write in t instead of a
    read in t, then it would have been possible for the loop to produce
    Uninit(t).

    TOMOVE global invariants of well-typed tree. Assertions to implement:
    - a RW resource cannot be used in JoinedFrac mode.
    - a resource coming from the invariant or consumes clause cannot
      be used in Produced mode.
    - a resource in consumes clause and usage None, must appear in
      produces clauses.

    Strategy to implement to help minimization:
    if the invariant takes as input RO(f,X) * RO(g,X),
    then only one of the two should be used by the body;
    this requires a consistent prioritization of the choice
    of the read-only permission in case there are multiple possibilities.

    ===========
    More advanced minimization, applied only with specific flags on,
    specifically for the treatment of fractions. This minimization attempts
    to change the produces clause of a contract in order to perform a
    "cancellation of fraction" outside of the loop instead of inside the loop.

    Consider the following example, which typically arises from a fission.

      for i
        consumes RO(f,Ai)
        f(i) // consumes RO(f,Ai) and produces RO(g,Bi) * RO(f-g,Ai)
        produces RO(g,Bi) * RO(f-g,Ai)
      for i
        consumes RO(g,Bi) * RO(f-g,Ai)
        g(i) // consumes RO(g,Bi) and produces RO(g,Ai)
        // cancel RO(g,Ai) * RO(f-g,Ai) into RO(f,Ai)
        produces RO(f,Ai)

    We would like to minimize the second loop:

      for i
        consumes RO(g,Bi)
        g(i) // consumes RO(g,Bi) and produces RO(g,Ai)
        produces RO(g,Ai)
      // outside the loop cancel: RO(g, stars_i Ai) * RO(f-g, stars_i Ai)
      // into: RO(f, stars_i Ai)

    ===========
    Dually, we way want to pull implicit RW->RO operation before the loop.

    Example:
      for i
        __xconsumes("&t[i] ~> Struct");
        __xproduces("RO(1 - f, &t[i] ~> Struct), RO(f, &t[i].k ~> Cell)");
        __ghost(focus_ro_k);

    we could do instead:

      for i
        __xconsumes("RO(f, &t[i] ~> Struct)");
        __xproduces("RO(f, &t[i].k ~> Cell)");
        __ghost(focus_ro_k);

    ===========


    Note: it may be tempting to generalize this idea to other examples.
    Consider for example:

      for i
        consumes Ai * (Bi \-* Ci)
        f(i) // consumes Ai produces Bi
        produces Ci

    It makes sense to minimize it to:

      for i
        consumes Ai
        f(i) // consumes Ai produces Bi
        produces Bi
      then entailement from (stars_i Bi) * stars_i (Bi \-* Ci) to (stars_i Ci)

    However, in this example we don't want to push the wand-cancel outside the loop

      for i
        consumes Ai * Di
        f(i) // consumes Ai produces (Bi \-* Ci)
        g(i) // consumes Di produces Bi
        produces Ci

    because this reformuation looks rather uninteresting
    and does not lead to a simpler "consumes" clause

      for i
        consumes Ai * Di
        f(i) // consumes Ai produces (Bi \-* Ci)
        g(i) // consumes Di produces Bi
        produces (Bi \-* Ci) * Bi
      then entailement from (stars_i Bi) * stars_i (Bi \-* Ci) to (stars_i Ci)

    Note that in this last example, a fission between f(i) and g(i)
    leads to the code in the previous example.
*)

let minimize_loop_contract contract post_inst usage =
  let new_fracs = ref [] in
  let new_linear_invariant, added_par_reads = List.fold_right (fun (hyp, formula) (lin_inv, par_reads) ->
    match Var_map.find_opt hyp usage with
    | None -> (lin_inv, par_reads)
    | Some (Required | Ensured) -> failwith "minimize_loop_contract: the linear resource %s is used like a pure resource" (var_to_string hyp)
    | Some (SplittedFrac | JoinedFrac) ->
      let { formula } = formula_read_only_inv_all formula in
      let frac_var, frac_ghost = new_frac () in
      new_fracs := frac_ghost :: !new_fracs;
      let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
      (lin_inv, (hyp, ro_formula) :: par_reads)
    | Some (ConsumedUninit | ConsumedFull) -> ((hyp, formula) :: lin_inv, par_reads)
    | Some Produced -> failwith "loop_minimize_on: Produced resource %s has the same id as a contract resource" (var_to_string hyp)
    ) contract.invariant.linear ([], [])
  in
  let new_invariant = { contract.invariant with linear = new_linear_invariant } in
  let new_parallel_reads = added_par_reads @ List.filter (fun (hyp, _) -> Var_map.mem hyp usage) contract.parallel_reads in

  let new_iter_contract = minimize_fun_contract ~output_new_fracs:new_fracs contract.iter_contract post_inst usage in

  let new_contract = { loop_ghosts = !new_fracs @ contract.loop_ghosts;
    invariant = new_invariant;
    parallel_reads = new_parallel_reads;
    iter_contract = new_iter_contract;
    strict = contract.strict }
  in
  let new_contract_used_vars = loop_contract_used_vars new_contract in

  let filter_pure_pre (x, formula) =
    Var_set.mem x new_contract_used_vars || (not (are_same_trm formula trm_frac) && Var_map.mem x usage)
  in

  { new_contract with
    loop_ghosts = List.filter filter_pure_pre new_contract.loop_ghosts;
    invariant = { new_contract.invariant with pure = List.filter filter_pure_pre new_contract.invariant.pure }
  }

let loop_minimize_on (t: trm): trm =
  let range, body, contract = trm_inv ~error:"loop_minimize_on: not a for loop" trm_for_inv t in

  let body_usage = usage_of_trm body in
  let post_inst = post_inst body in
  let new_contract = minimize_loop_contract contract post_inst body_usage in
  trm_like ~old:t (trm_for range ~contract:new_contract body)

(* [loop_minimize]: minimize linear invariants of a loop contract *)
let%transfo loop_minimize (*?(indepth : bool = false)*) (tg: target) : unit =
  ensure_computed ();
  (* TODO: Perform minimization recursively when indepth is true. *)
  Target.apply_at_target_paths loop_minimize_on tg;
  justif_correct "only changed loop contracts"


let make_strict_loop_contract_on (t: trm): trm =
  let range, body, contract = trm_inv ~error:"make_strict_loop_contract_on: not a for loop" trm_for_inv t in
  if contract.strict then trm_fail t "make_strict_loop_contract_on: the loop already has a strict contract";

  let body_res_usage = usage_of_trm body in
  let { contract_frame } = unsome_or_trm_fail t "make_strict_loop_contract_on: resources need to be computed before this transformation" t.ctx.ctx_resources_contract_invoc in

  let fracs = ref [] in
  let lin_invariant = ref [] in
  let parallel_reads = ref [] in
  List.iter (fun (x, f) ->
    match Var_map.find_opt x body_res_usage with
    | None -> ()
    | Some (ConsumedFull | ConsumedUninit) ->
      lin_invariant := (new_anon_hyp (), f) :: !lin_invariant;
    | Some (SplittedFrac | JoinedFrac) ->
      let frac, frac_item = new_frac () in
      fracs := frac_item :: !fracs;
      let { formula = f } = formula_read_only_inv_all f in
      parallel_reads := (new_anon_hyp (), formula_read_only ~frac:(trm_var frac) f) :: !parallel_reads
    | Some _ -> failwith "Found a usage incompatible with the loop contract frame")
    contract_frame;

  let contract = {
    loop_ghosts = (List.rev !fracs) @ contract.loop_ghosts;
    invariant = Resource_set.add_linear_list (List.rev !lin_invariant) contract.invariant;
    parallel_reads = (List.rev !parallel_reads) @ contract.parallel_reads;
    iter_contract = contract.iter_contract;
    strict = true }
  in
  trm_alter ~desc:(Trm_for (range, body, contract)) t

let rec make_strict_loop_contract_rec (t: trm): trm =
  match t.ctx.ctx_resources_before with
  | None -> t
  | Some _ ->
    let t = trm_map ~keep_ctx:true make_strict_loop_contract_rec t in
    match t.desc with
    | Trm_for (_, _, contract) when not contract.strict ->
      make_strict_loop_contract_on t
    | _ -> t

(** [make_strict_loop_contracts] uses computed resources to fix the default loop contracts on all the children of the targeted node *)
let%transfo make_strict_loop_contracts (tg: target): unit =
  ensure_computed ();
  Target.apply_at_target_paths make_strict_loop_contract_rec tg;
  justif_correct "only changed loop contracts"

type unparsed_fun_contract = (fun_contract_clause_type * string) list
type unparsed_loop_contract = (loop_contract_clause_type * string) list

let with_desugared_res push_fn clause (x, formula) contract =
  push_fn clause (x, (desugar_formula (Ast_fromto_AstC.caddress_elim formula))) contract

let parse_fun_contract =
  parse_contract_clauses empty_fun_contract (with_desugared_res push_fun_contract_clause)

let parse_loop_contract ~strict =
  parse_contract_clauses (if strict then empty_strict_loop_contract else empty_loop_contract) (with_desugared_res push_loop_contract_clause)

let __pure = Requires, ""
let __requires (r: string) = Requires, r
let __ensures (r: string) = Ensures, r
let __reads (r: string) = Reads, r
let __writes (r: string) = Writes, r
let __modifies (r: string) = Modifies, r
let __consumes (r: string) = Consumes, r
let __produces (r: string) = Produces, r

let __loop_requires (r: string) = LoopVars, r
let __xrequires (r: string) = Exclusive Requires, r
let __xensures (r: string) = Exclusive Ensures, r
let __xreads (r: string) = Exclusive Reads, r
let __xwrites (r: string) = Exclusive Writes, r
let __xmodifies (r: string) = Exclusive Modifies, r
let __xconsumes (r: string) = Exclusive Consumes, r
let __xproduces (r: string) = Exclusive Produces, r
let __invariant (r: string) = Invariant, r
let __sreads (r: string) = SharedReads, r
let __smodifies (r: string) = SharedModifies, r


let%transfo delete_annots (tg : Target.target) : unit =
  Trace.justif "changes only annotations";
  Nobrace_transfo.remove_after (fun () ->
    Target.apply_at_target_paths Resource_trm.delete_annots_on tg;
    Show.ast ()
  )

let set_fun_contract_on (contract: fun_contract) (t: trm): trm =
  let name, ret_typ, args, body = trm_inv ~error:"Resources.set_fun_contract_on: Expected function" trm_let_fun_inv t in
  trm_like ~old:t (trm_let_fun name ret_typ args ~contract:(FunSpecContract contract) body)

let%transfo set_fun_contract (contract: unparsed_fun_contract) (tg : Target.target) : unit =
  Target.apply_at_target_paths (set_fun_contract_on (parse_fun_contract contract)) tg

let set_loop_contract_on (contract: loop_contract) (t: trm): trm =
  let range, body, _ = trm_inv ~error:"Resource.set_loop_contract_on: Expected for loop" trm_for_inv t in
  trm_like ~old:t (trm_for ~contract range body)

let%transfo set_loop_contract ?(strict:bool=true) (contract: unparsed_loop_contract) (tg: Target.target): unit =
  Target.apply_at_target_paths (set_loop_contract_on (parse_loop_contract ~strict contract)) tg;
  if not strict then begin
    ensure_computed ();
    Target.apply_at_target_paths make_strict_loop_contract_on tg
  end


let ghost_group_ro_focus = toplevel_var "group_ro_focus"

let detach_loop_ro_focus_on (t: trm): trm =
  let range, body, contract = trm_inv ~error:"detach_loop_ro_focus_on: not a for loop" trm_for_inv t in
  let iter_reads, iter_pre, iter_post = filter_common_resources ~filter_map_left:(fun formula -> Option.map (fun _ -> formula) (formula_read_only_inv formula)) contract.iter_contract.pre.linear contract.iter_contract.post.linear in
  let new_par_reads = List.map (fun (x, formula) -> (x, formula_group_range range formula)) iter_reads in
  let contract = { contract with
    iter_contract = {
      pre = { contract.iter_contract.pre with linear = iter_pre };
      post = { contract.iter_contract.post with linear = iter_post }
    };
    parallel_reads = new_par_reads @ contract.parallel_reads
  } in
  let new_body = trm_seq_nobrace (trm_inv trm_seq_inv body) in
  let new_body = List.fold_right (fun (_, formula) ->
    let { formula } = Option.get (formula_read_only_inv formula) in
    let i = new_var range.index.name in
    let items = formula_fun [i, typ_int ()] None (trm_subst_var range.index (trm_var i) formula) in
    Resource_trm.ghost_scope (ghost_call ghost_group_ro_focus ["i", (trm_var range.index); "items", items])) iter_reads new_body
  in
  let new_body = trm_like ~old:body new_body in
  trm_like ~old:t (trm_for range ~contract new_body)

(* [detach_loop_ro_focus tg] transforms all the ressources that are in a reads clause into a resource in a par_read clause with a focus around the loop body. *)
let%transfo detach_loop_ro_focus (tg: target): unit =
  Nobrace_transfo.remove_after (fun () ->
    Target.apply_at_target_paths detach_loop_ro_focus_on tg
  );
  justif_correct "only changed loop contracts"


let assert_hyp_read_only ~(error : string) ((x, t) : resource_item) : unit =
  match formula_read_only_inv t with
  | Some _ -> ()
  | None -> failwith "%s: %s is used sequentially and is not read only." error (Ast_fromto_AstC.(named_formula_to_string (default_style ())) (x, t))

let justif_parallelizable_loop_contract ~error (contract: loop_contract): unit =
  if contract.invariant.linear <> []
    then failwith "%s: the for loop is not parallelizable, invariant non empty." error
    else Trace.justif "The for loop is parallelizable"

(** Collects effects intererences as a map from hyps to pairs of interfering resource usages. *)
(* TODO: is the [before] / [after] relationship still required? *)
let collect_interferences (before : resource_usage_map) (after : resource_usage_map) : (resource_usage option * resource_usage option) Var_map.t =
  let res_merge _ a_res_usage b_res_usage =
    match (a_res_usage, b_res_usage) with
    | _, None
    | None, _ -> None
    | Some (Ensured | Produced | ConsumedFull | ConsumedUninit), _
    | _, Some (Ensured | Produced | ConsumedFull | ConsumedUninit) -> Some (a_res_usage, b_res_usage)
    | _ -> None
  in
  Var_map.merge res_merge before after

(** Collects effects intererences as a map from hyps to pairs of interfering resource usages. *)
(* TODO: is the [before] / [after] relationship still required? *)
let collect_trm_interferences (before : trm) (after : trm) : (resource_usage option * resource_usage option) Var_map.t =
  collect_interferences (usage_of_trm before) (usage_of_trm after)

(** <private> *)
let string_of_interference (interference : (resource_usage option * resource_usage option) Var_map.t) : string =
  sprintf "the resources do not commute: %s\n" (Tools.list_to_string (List.map (fun (x, (f1, f2)) -> sprintf "%s: %s != %s" x.name (resource_usage_opt_to_string f1) (resource_usage_opt_to_string f2)) (Var_map.bindings interference)))

(** Checks that resource usages commute, infer var ids to check pure facts scope. *)
let assert_usages_commute (loc : location) (before : resource_usage_map) (after : resource_usage_map) : unit =
  let interference = collect_interferences before after in
  if not (Var_map.is_empty interference) then
    loc_fail loc (string_of_interference interference)

(** Checks that the effects from the instruction at path [p] are shadowed by following effects
   in the program.

   - if provided [pred], allows specifying which formulas to include in the shadowing check.
    *)
let assert_instr_effects_shadowed ?(pred : formula -> bool = fun _ -> true) (p : path) : unit =
  step_backtrack ~discard_after:true (fun () ->
    Nobrace_transfo.remove_after ~check_scoping:false (fun () ->
      Target.apply_at_path (fun instr ->
        let res_before = before_trm instr in
        let write_res = List.filter (Resource_set.(linear_usage_filter (usage_of_trm instr) keep_written)) res_before.linear in
        let write_res = List.map (fun (_, formula) -> formula) write_res in
        let write_res = List.filter pred write_res in
        let uninit_ghosts = List.filter_map (fun res ->
          if Option.is_none (formula_uninit_inv res) then Some (Resource_trm.ghost_forget_init res) else None) write_res in
        trm_seq_nobrace_nomarks uninit_ghosts
      ) p
    );
    recompute_resources ()
  )


(** <private>
    Checks that the trm resource usage does not contain any resources used Full or Produced.
    This corresponds to self interference of the trm:
    A trm is not self interfere if `t; t` is the same as `t`
*)
let assert_not_self_interfering (t : trm) : unit =
  let res_before = before_trm t in
  let res_after = after_trm t in
  let res_usage = usage_of_trm t in
  let res_used_uninit = List.filter (fun (h, f) ->
    match Var_map.find_opt h res_usage with
    | Some ConsumedFull -> trm_fail t "trm has self interfering resource usage"
    | Some ConsumedUninit -> true
    | Some (SplittedFrac|JoinedFrac) | None -> false
    | Some (Produced|Required|Ensured) -> trm_fail t "trm has invalid resource usage"
  ) res_before.linear in
  let res_produced = List.filter (fun (h, f) ->
    match Var_map.find_opt h res_usage with
    | Some Produced -> true
    | Some (SplittedFrac|JoinedFrac) | None -> false
    | Some (ConsumedFull|ConsumedUninit|Required|Ensured) -> trm_fail t "trm has invalid resource usage"
  ) res_after.linear in
  ignore (Resource_computation.subtract_linear_resource_set res_produced res_used_uninit)

(** Checks that duplicating the instruction at index [index] after [skip] instructions in the sequence [seq] would be redundant.

  instr; // reads b, consumes uninit a, produces RW a, possibly with multiple a's and b's
  other_instr; // must not write a or b = must use a and b only in RO
  instr; // exactly the same instruction as above including ghosts args --> can be deleted because it will produce the same W value from the same R dependencies
  *)
let assert_dup_instr_redundant (index : int) (skip : int) (seq : trm) : unit =
  let instrs = trm_inv ~error:"Resources.assert_instr_redundant: expected sequence" trm_seq_inv seq in
  let useful_instrs = Xlist.take (skip + 1) (Xlist.drop index (Mlist.to_list instrs)) in
  let instr, other_instrs = Xlist.extract_element useful_instrs 0 in
  assert_not_self_interfering instr;
  let res = usage_of_trm instr in
  let usage_interferes hyp res_usage =
    match res_usage with
    | Required | Ensured | SplittedFrac | JoinedFrac -> false
    | ConsumedFull | ConsumedUninit | Produced -> Var_map.mem hyp res
  in
  let instr_interference i t =
    let interferences = Var_map.filter usage_interferes (usage_of_trm t) in
    if Var_map.is_empty interferences then None else Some (i, interferences)
  in
  let interferences = List.filter_map (fun x -> x) (List.mapi instr_interference other_instrs) in
  if interferences <> [] then
    (* TODO: better error message: formulas? *)
    trm_fail seq (sprintf "Resources.assert_instr_redundant: not redundant due to resources %s\n" (Tools.list_to_string (List.concat_map (fun (i, usage) ->
      List.map (fun (x, u) ->
        sprintf "conflict with instr %d on %s (%s)" (i + 1) x.name (resource_usage_opt_to_string (Some u))
      ) (Var_map.bindings usage)
    ) interferences)));
  ()

(* TEMPORARY for backward compatibility -- TODO: substitute them in tests *)
let show () =
  ensure_computed ();
  Show.res ()

let show_ast () = Show.ast ()
