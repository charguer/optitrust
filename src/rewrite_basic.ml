
(* [equiv_at rule]: apply rule [rule]  on target [tg]*)
let equiv_at (rule : string) : Target.Transfo.t = 
  let rule_descr = Rewrite_core.parse_rule rule in
  Target.apply_on_targets (Rewrite_core.apply_rule rule_descr)


(* [compute tg]: expects the target [tg] to point to an arithmetic operation
  then it will try to simlplify it if it falls in the supported categories
*)
let compute : Target.Transfo.t = 
  Target.apply_on_targets (Rewrite_core.compute)




