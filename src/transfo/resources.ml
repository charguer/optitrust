open Ast
open Target
open Resources_contract

let set_fun_contract_on (contract: fun_contract) (t: trm): trm =
  let name, ret_typ, args, body = trm_inv ~error:"Resources.set_fun_contract_on: Expected function" trm_let_fun_inv t in
  trm_like ~old:t (trm_let_fun (qvar_to_var name) ret_typ args ~contract body)

let set_fun_contract (contract: fun_contract) (tg : Target.target) : unit =
  Target.apply_at_target_paths (set_fun_contract_on contract) tg

(*let recompute_resources (tg : Target.target) : unit =
  Target.apply_at_target_paths (Resources_core.trm_recompute_resources) tg*)

let recompute_all_resources () : unit =
  let t = Trace.ast () in
  (* TODO: Configurable base environment *)
  let t = Resources_computation.(trm_recompute_resources builtin_env t) in
  Trace.set_ast t

let trm_for_inv_contract t =
  match t.desc with
  | Trm_for (range, body, contract) -> Some (range, body, contract)
  | _ -> None

let loop_minimize_on (t: trm): trm =
  let range, body, contract = trm_inv ~error:"loop_minimize_on: not a for loop" trm_for_inv_contract t in
  let res_before =
    match t.ctx.ctx_resources_before with
    | None -> fail t.loc "loop_minimize_on: resources need to be computed before this transformation"
    | Some res_before -> res_before
  in

  let contract = match contract with
    | None -> { loop_ghosts = res_before.pure; invariant = { res_before with pure = [] }; iter_contract = empty_fun_contract }
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
  Resources_computation.(trm_recompute_resources res_before t)

(* [loop_minimize]: minimize linear invariants of a loop contract *)
let%transfo loop_minimize (tg: target) : unit =
  recompute_all_resources (); (* TODO: Incrementalize this computation *)
  Target.apply_at_target_paths loop_minimize_on tg
