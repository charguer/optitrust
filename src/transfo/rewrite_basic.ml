open Prelude
open Target

(** [equiv_at rule]: expects the target [tg] to point at a trm where [rule] can be applied to transform
   that trm into a similar one defined by the rule itself. *)
let equiv_at ?(mark : mark = no_mark) ?(glob_defs : string = "") ?(ctx : bool = false) ?(indepth = false) (rule : string) : target -> unit =
  let rule = Trm_matching.parse_rule ~ctx ~glob_defs rule in
  Target.apply_at_target_paths (if indepth then Rewrite_core.apply_rule_bottom_up ~mark rule else Rewrite_core.apply_rule_on ~mark ~error_msg:true rule)

(** [compute tg]: expects the target [tg] to point at an arithmetic operation then it will try to simlplify it. *)
let compute : target -> unit =
  Target.apply_at_target_paths (Rewrite_core.compute_on)

(** [compute_inside tg]: expects the target [tg] to point at any trm in the ast that could contain some inner
   arithmetic operations, then it will try to simplify them by calling [compute] on that trm. *)
let compute_inside (tg : Target.target) : unit =
  let tg =
    if List.exists (function Constr.Constr_occurrences _ -> true | _ -> false) tg
      then tg
      else Target.nbMulti::tg in
  let cPrimFunAny = Target.cPrimPredFun (function (Prim_binop _) | (Prim_unop _ )-> true | _ -> false) in
  let new_tg = tg @ [cPrimFunAny] in
  Target.apply_at_target_paths (Rewrite_core.compute_on) new_tg
