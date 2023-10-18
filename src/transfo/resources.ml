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

(*let recompute_resources (tg : Target.target) : unit =
  Target.apply_at_target_paths (Resources_core.trm_recompute_resources) tg*)

let recompute_all_resources () : unit =
  Trace.typing_step ~name:"Resource recomputation" (fun () ->
    let t = Trace.ast () in
    let t = Scope.infer_var_ids t in (* Resource computation needs var_ids to be calculated *)
    (* TODO: Configurable base environment *)
    let t = Resource_computation.(trm_recompute_resources empty_resource_set t) in
    Trace.set_ast t
  )

(* TODO: avoid recomputing all resources for validity checks. *)
let required_for_check () : unit =
  if !Flags.check_validity then recompute_all_resources ()

let justif_correct (why : string) : unit =
  if !Flags.check_validity then begin
    recompute_all_resources ();
    Trace.justif (sprintf "resources are correct: %s" why)
  end

let trm_for_contract t =
  match trm_for_inv t with
  | Some (_, _, Some c) -> Some c
  | _ -> None

let loop_minimize_on (t: trm): trm =
  let range, body, contract = trm_inv ~error:"loop_minimize_on: not a for loop" trm_for_inv t in
  let res_before =
    match t.ctx.ctx_resources_before with
    | None -> fail t.loc "loop_minimize_on: resources need to be computed before this transformation"
    | Some res_before -> res_before
  in

  let contract = match contract with
    | None -> { loop_ghosts = res_before.pure; invariant = resource_set ~linear:res_before.linear (); iter_contract = empty_fun_contract }
    | Some contract -> contract
  in

  let body_res_usage =
    match body.ctx.ctx_resources_usage with
    | None -> fail t.loc "loop_minimize_on: the body of the loop needs a resource usage annotation"
    | Some res_before -> res_before
  in

  let new_fracs = ref [] in
  let keep_used_filter (hyp, formula) =
    match Hyp_map.find hyp body_res_usage with
    | NotUsed -> None
    | UsedReadOnly ->
      begin match formula_read_only_inv formula with
      | Some _ -> Some (hyp, formula)
      | None ->
        let frac_var, frac_ghost = new_frac () in
        new_fracs := frac_ghost :: !new_fracs;
        let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
        Some (hyp, ro_formula)
      end
    | UsedFull -> Some (hyp, formula)
  in

  let new_linear_invariant = List.filter_map keep_used_filter contract.invariant.linear in
  let new_invariant = { contract.invariant with linear = new_linear_invariant } in

  let new_contract = { contract with loop_ghosts = !new_fracs @ contract.loop_ghosts; invariant = new_invariant } in
  let t = trm_like ~old:t (trm_for range ~contract:new_contract body) in
  Resource_computation.(trm_recompute_resources res_before t)

(* [loop_minimize]: minimize linear invariants of a loop contract *)
let%transfo loop_minimize (tg: target) : unit =
  recompute_all_resources (); (* TODO: Incrementalize this computation *)
  Target.apply_at_target_paths loop_minimize_on tg

let assert_hyp_read_only ~(error : string) ((x, t) : (hyp * formula)) : unit =
  match formula_read_only_inv t with
  | Some _ -> ()
  | None -> failwith (sprintf "%s: %s is used sequentially and is not read only." error (Ast_fromto_AstC.named_formula_to_string (x, t)))

let justif_parallelizable_loop_contract ~error (contract: loop_contract): unit =
  if contract.invariant.linear <> []
    then failwith (sprintf "%s: the for loop is not parallelizable, invariant non empty." error)
    else Trace.justif "The for loop is parallelizable"

(* checks that effects commute, infer var ids to check pure facts scope. *)
let assert_commute (before : trm) (after : trm) : unit =
  (* TODO: let error' = error in *)
  let error = "expected resources usage to be available" in
  let a_res = Tools.unsome ~error before.ctx.ctx_resources_usage in
  let b_res = Tools.unsome ~error after.ctx.ctx_resources_usage in
  let res_merge _ a_res_usage b_res_usage =
    match (a_res_usage, b_res_usage) with
    | (_, None)
    | (_, Some NotUsed)
    | (Some NotUsed, _) -> None
    | (None, _)
    | (Some UsedFull, _)
    | (_, Some UsedFull) -> Some (a_res_usage, b_res_usage)
    | _ -> None
  in
  let interference = Hyp_map.merge res_merge a_res b_res in
  if not (Hyp_map.is_empty interference) then
    fail after.loc (sprintf "the resources do not commute: %s\n" (Tools.list_to_string (List.map (fun (x, (f1, f2)) -> sprintf "%s: %s != %s" x.name (resource_usage_opt_to_string f1) (resource_usage_opt_to_string f2)) (Hyp_map.bindings interference))))

(* [show] enables to view the result of resource computations. *)
let show (*LATER?(details:bool=true)*) ?(line:int = -1) () : unit =
  let t = Trace.ast() in
  let t = Scope.infer_var_ids t in (* Resource computation needs var_ids to be calculated *)
  let tres = Resource_computation.(trm_recompute_resources empty_resource_set t) in
  show_computed_res ~line ~ast:tres ()
