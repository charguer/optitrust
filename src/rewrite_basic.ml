(* [equiv_at rule]: apply rule [rule]  on target [tg]*)
let equiv_at (rule : string) : Target.Transfo.t = 
  Target.reparse_after ~reparse:false (
  let rule_descr = Rewrite_core.parse_rule rule in
  Target.apply_on_targets (Rewrite_core.apply_rule rule_descr))

(* [compute tg]: expects the target [tg] to point to an arithmetic operation
  then it will try to simlplify it if it falls in the supported categories
*)
let compute : Target.Transfo.t = 
  Target.apply_on_targets (Rewrite_core.compute)


(* [compute_inside tg]: expects the target [tg] to point any node of the ast which could contain some inner arithmetic operations
  then it will try to simplify them by calling compute in tha node
*)
let compute_inside (tg : Target.target) : unit = 
  let tg =
    if List.exists (function Constr.Constr_occurrences _ -> true | _ -> false) tg
      then tg
      else Target.nbMulti::tg in
  let cPrimFunAny = Target.cPrimPredFun (function (Prim_binop _) | (Prim_unop _ )-> true | _ -> false) in
  let new_tg = tg @ [cPrimFunAny] in
  Target.apply_on_targets (Rewrite_core.compute) new_tg




