open Prelude
open Target
open Resource_formula
open Resource_contract

type unparsed_contract = (contract_clause_type * string) list

let with_desugared_res push_fn clause (x, formula) contract =
  push_fn clause (x, (desugar_formula (Ast_fromto_AstC.caddress_elim formula))) contract

let parse_fun_contract =
  parse_contract_clauses empty_fun_contract (with_desugared_res push_fun_contract_clause)

let parse_loop_contract =
  parse_contract_clauses empty_loop_contract (with_desugared_res push_loop_contract_clause)

let __pure () = Requires, ""
let __requires (r: string) = Requires, r
let __ensures (r: string) = Ensures, r
let __invariant (r: string) = Invariant, r
let __reads (r: string) = Reads, r
let __writes (r: string) = Writes, r
let __modifies (r: string) = Modifies, r
let __consumes (r: string) = Consumes, r
let __produces (r: string) = Produces, r
let __sequentially_reads (r: string) = SequentiallyReads, r
let __sequentially_modifies (r: string) = SequentiallyModifies, r

let set_fun_contract_on (contract: fun_contract) (t: trm): trm =
  let name, ret_typ, args, body = trm_inv ~error:"Resources.set_fun_contract_on: Expected function" trm_let_fun_inv t in
  trm_like ~old:t (trm_let_fun name ret_typ args ~contract:(FunSpecContract contract) body)

let%transfo set_fun_contract (contract: unparsed_contract) (tg : Target.target) : unit =
  Target.apply_at_target_paths (set_fun_contract_on (parse_fun_contract contract)) tg

let set_loop_contract_on (contract: loop_contract) (t: trm): trm =
  let range, body, _ = trm_inv ~error:"Resource.set_loop_contract_on: Expected for loop" trm_for_inv t in
  trm_like ~old:t (trm_for ~contract range body)

let%transfo set_loop_contract (contract: unparsed_contract) (tg: Target.target): unit =
  Target.apply_at_target_paths (set_loop_contract_on (parse_loop_contract contract)) tg


let ensure_computed () =
  if not !Flags.recompute_resources_between_steps then Trace.recompute_resources ()

(* TODO: avoid recomputing all resources for validity checks. *)
let required_for_check () : unit =
  if !Flags.check_validity then ensure_computed ()

let justif_correct (why : string) : unit =
  if !Flags.check_validity then begin
    ensure_computed ();
    Trace.justif (sprintf "resources are correct: %s" why)
  end

let trm_for_contract t =
  match trm_for_inv t with
  | Some (_, _, Some c) -> Some c
  | _ -> None

(** Returns the resource usage of the given term, fails if unavailable. *)
let usage_of_trm (t : trm) =
  unsome_or_trm_fail t "expected resource usage to be available" t.ctx.ctx_resources_usage

(** Computes the resource usage of a consecutive sequence of instructions. *)
let compute_usage_of_instrs (instrs : trm list) : resource_usage_map =
  List.fold_left (fun usage_map t ->
    Resource_computation.update_usage_map ~current_usage:usage_map ~extra_usage:(usage_of_trm t)
  ) Resource_computation.empty_usage_map instrs

type usage_filter =
 { unused: bool; read_only: bool; uninit: bool; full: bool; produced: bool; }

let keep_all = { unused = true; read_only = true; uninit = true; full = true; produced = true; }
let keep_none = { unused = false; read_only = false; uninit = false; full = false; produced = false; }
let keep_touched = { keep_all with unused = false; }
let keep_used = { keep_touched with produced = false; }
let keep_produced = { keep_none with produced = true; }
let keep_unused = { keep_none with unused = true; }
let keep_written = { keep_used with read_only = false; }

(** A filter compatible with [List.filter] or [List.partition] that selects resources by their usage in the usage map given. *)
let usage_filter usage filter (h, _) =
  match Var_map.find_opt h usage with
  | None -> filter.unused
  | Some UsedReadOnly -> filter.read_only
  | Some UsedUninit -> filter.uninit
  | Some UsedFull -> filter.full
  | Some Produced -> filter.produced


let loop_minimize_on (t: trm): trm =
  let range, body, contract = trm_inv ~error:"loop_minimize_on: not a for loop" trm_for_inv t in
  let res_before =
    match t.ctx.ctx_resources_before with
    | None -> trm_fail t "loop_minimize_on: resources need to be computed before this transformation"
    | Some res_before -> res_before
  in

  let contract = match contract with
    | None -> { loop_ghosts = res_before.pure; invariant = Resource_set.make ~linear:res_before.linear (); iter_contract = empty_fun_contract }
    | Some contract -> contract
  in

  let body_res_usage = usage_of_trm t in

  let new_fracs = ref [] in
  let keep_used_filter (hyp, formula) =
    match Hyp_map.find_opt hyp body_res_usage with
    | None -> None
    | Some UsedReadOnly ->
      begin match formula_read_only_inv formula with
      | Some _ -> Some (hyp, formula)
      | None ->
        let frac_var, frac_ghost = new_frac () in
        new_fracs := frac_ghost :: !new_fracs;
        let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
        Some (hyp, ro_formula)
      end
    | Some (UsedUninit | UsedFull) -> Some (hyp, formula)
    | Some Produced -> failwith (sprintf "loop_minimize_on: Produced resource %s has the same id as a contract resource" (var_to_string hyp))
  in

  let new_linear_invariant = List.filter_map keep_used_filter contract.invariant.linear in
  let new_invariant = { contract.invariant with linear = new_linear_invariant } in

  let new_contract = { contract with loop_ghosts = !new_fracs @ contract.loop_ghosts; invariant = new_invariant } in
  let t = trm_like ~old:t (trm_for range ~contract:new_contract body) in
  Resource_computation.(trm_recompute_resources res_before t)

(* [loop_minimize]: minimize linear invariants of a loop contract *)
let%transfo loop_minimize (tg: target) : unit =
  ensure_computed ();
  Target.apply_at_target_paths loop_minimize_on tg

let assert_hyp_read_only ~(error : string) ((x, t) : resource_item) : unit =
  match formula_read_only_inv t with
  | Some _ -> ()
  | None -> failwith (sprintf "%s: %s is used sequentially and is not read only." error (Ast_fromto_AstC.named_formula_to_string (x, t)))

let justif_parallelizable_loop_contract ~error (contract: loop_contract): unit =
  if contract.invariant.linear <> []
    then failwith (sprintf "%s: the for loop is not parallelizable, invariant non empty." error)
    else Trace.justif "The for loop is parallelizable"

(** Collects effects intererences as a map from hyps to pairs of interfering resource usages. *)
(* TODO: is the [before] / [after] relationship still required? *)
let collect_interferences (before : resource_usage_map) (after : resource_usage_map) : (resource_usage option * resource_usage option) Hyp_map.t =
  let res_merge _ a_res_usage b_res_usage =
    match (a_res_usage, b_res_usage) with
    | (_, None)
    | (None, _) -> None
    | (Some (Produced | UsedFull | UsedUninit), _)
    | (_, Some (Produced | UsedFull | UsedUninit)) -> Some (a_res_usage, b_res_usage)
    | _ -> None
  in
  Hyp_map.merge res_merge before after

(** Collects effects intererences as a map from hyps to pairs of interfering resource usages. *)
(* TODO: is the [before] / [after] relationship still required? *)
let collect_trm_interferences (before : trm) (after : trm) : (resource_usage option * resource_usage option) Hyp_map.t =
  collect_interferences (usage_of_trm before) (usage_of_trm after)

(** <private> *)
let string_of_interference (interference : (resource_usage option * resource_usage option) Hyp_map.t) : string =
  sprintf "the resources do not commute: %s\n" (Tools.list_to_string (List.map (fun (x, (f1, f2)) -> sprintf "%s: %s != %s" x.name (resource_usage_opt_to_string f1) (resource_usage_opt_to_string f2)) (Hyp_map.bindings interference)))

(** Checks that resource usages commute, infer var ids to check pure facts scope. *)
let assert_usages_commute (loc : location) (before : resource_usage_map) (after : resource_usage_map) : unit =
  let interference = collect_interferences before after in
  if not (Hyp_map.is_empty interference) then
    loc_fail loc (string_of_interference interference)

(** Checks that effects commute, infer var ids to check pure facts scope. *)
let assert_seq_instrs_commute (before : trm) (after : trm) : unit =
  let interference = collect_trm_interferences before after in
  if not (Hyp_map.is_empty interference) then
    trm_fail after (string_of_interference interference)

(** <private>
    Collects the resources that are consumed Full or Uninit (i.e. possibly written to) by term [t].
    *)
let write_usage_of (t : trm) : hyp list =
  let res = usage_of_trm t in
  let keep hyp res_usage =
    match res_usage with
    | UsedFull | UsedUninit-> true
    | UsedReadOnly | Produced  -> false
  in
  let write_res = Hyp_map.filter keep res in
  List.map (fun (h, _) -> h) (Hyp_map.bindings write_res)

(** <private>
    Returns a list of formulas from a list of hypothesis variables.
    *)
let formulas_of_hyps (hyps: hyp list) (resources: resource_item list): formula list =
  let hyp_map = Hyp_map.of_seq (List.to_seq resources) in
  List.map (fun h -> Hyp_map.find h hyp_map) hyps

(** Checks that the effects from the instruction at path [p] are shadowed by following effects in the program.
    *)
let assert_instr_effects_shadowed (p : path) : unit =
  step_to_cancel ~discard_after:true (fun () ->
    Nobrace_transfo.remove_after ~check_scoping:false (fun () ->
      Target.apply_at_path (fun instr ->
        let write_hyps = write_usage_of instr in
        let res_before = unsome_or_trm_fail instr "Resources.assert_instr_effects_shadowed: expected resources to be computed" instr.ctx.ctx_resources_before in
        let write_res = formulas_of_hyps write_hyps res_before.linear in
        let uninit_ghosts = List.filter_map (fun res ->
          if Option.is_none (formula_uninit_inv res) then Some (trm_ghost_forget_init res) else None) write_res in
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
  let res_before = Xoption.unsome ~error:"expected computed resources" t.ctx.ctx_resources_before in
  let res_after = Xoption.unsome ~error:"expected computed resources" t.ctx.ctx_resources_after in
  let res_usage = usage_of_trm t in
  let res_used_uninit = List.filter (fun (h, f) ->
    match Hyp_map.find_opt h res_usage with
    | Some UsedFull -> trm_fail t "trm has self interfering resource usage"
    | Some UsedUninit -> true
    | Some UsedReadOnly | None -> false
    | Some Produced -> trm_fail t "trm has invalid resource usage"
  ) res_before.linear in
  let res_produced = List.filter (fun (h, f) ->
    match Hyp_map.find_opt h res_usage with
    | Some Produced -> true
    | Some UsedReadOnly | None -> false
    | Some (UsedFull|UsedUninit) -> trm_fail t "trm has invalid resource usage"
  ) res_after.linear in
  ignore (Resource_computation.subtract_linear_resource res_produced res_used_uninit)

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
  let usage_does_not_interfere hyp res_usage =
    let interferes = Hyp_map.mem hyp res && res_usage <> UsedReadOnly in
    not interferes
  in
  let instr_does_dot_interfere t =
    Hyp_map.for_all usage_does_not_interfere (usage_of_trm t)
  in
  let is_redundant = List.for_all instr_does_dot_interfere other_instrs in
  if not is_redundant then
    trm_fail seq "Resources.assert_instr_redundant: not redundant";
  ()

(* [show] enables to view the result of resource computations. *)
let show (*LATER?(details:bool=true)*) ?(line:int = -1) () : unit =
  step_to_cancel ~discard_after:true (fun () ->
    recompute_resources ();
    show_computed_res ~line ()
  )
