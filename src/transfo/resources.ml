open Ast

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
  let t = Resources_core.(trm_recompute_resources builtin_env t) in
  Trace.set_ast t
