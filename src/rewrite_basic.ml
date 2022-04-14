(* [equiv_at rule]: apply rule [rule]  on target [tg]*)
let equiv_at ?(glob_defs : string = "") ?(ctx : bool = false) (rule : string) : Target.Transfo.t = 
  Target.reparse_after ~reparse:true (
  let rule_descr = Trm_matching.parse_rule ~ctx ~glob_defs rule in
  Target.apply_on_targets (Rewrite_core.apply_rule rule_descr))

(* [compute tg]: expects the target [tg] to point at an arithmetic operation
  then it will try to simlplify it if the operation falls in the supported categories *)
let compute : Target.Transfo.t = 
  Target.apply_on_targets (Rewrite_core.compute)


(* [compute_inside tg]: expects the target [tg] to point at any node of the ast that could contain some inner arithmetic operations
  then it will try to simplify them by calling compute in that node *)
let compute_inside (tg : Target.target) : unit = 
  let tg =
    if List.exists (function Constr.Constr_occurrences _ -> true | _ -> false) tg
      then tg
      else Target.nbMulti::tg in
  let cPrimFunAny = Target.cPrimPredFun (function (Prim_binop _) | (Prim_unop _ )-> true | _ -> false) in
  let new_tg = tg @ [cPrimFunAny] in
  Target.apply_on_targets (Rewrite_core.compute) new_tg
