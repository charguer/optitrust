
(* [equiv_at rule]: apply rule [rule]  on target [tg]*)
let equiv_at (rule : string) : Target.Transfo.t = 
  let rule_descr = Rewrite_core.parse_rule rule in
  Target.apply_on_targets (Rewrite_core.apply_rule rule_descr)


