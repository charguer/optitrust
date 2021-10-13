include Rewrite_basic

(* [equiv_at rule]: apply rule [rule]  on target [tg]*)
let equiv_at (rule : string) : Target.Transfo.t = 
  let rule_descr = Rewrite_basic.parse_rule rule in
  Target.apply_on_targets (Target.apply_on_path (Rewrite_basic.apply_rule rule_descr))

  