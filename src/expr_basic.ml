open Ast
open Target



(* [update f tg]: apply the operation [f] at the target [tg] *)
let update ?(reparse: bool = false)  (f : trm -> trm) : Target.Transfo.t =
  Target.reparse_after ~reparse (Target.apply_on_targets (Expr_core.update f))


(* [replace node tg]: expects the target to point at an instruction, then it will replace this
    instruction with [node]. Note that [node] can be also some code entered as string if that is 
    the case then to integrate it on the current ast this transformation shoudl be called with the flag ~reparse:true
   
   @correctness: Needs local manual reproving that if an invariant in the
   previous proof was { H } old_expr { Q } then { H } new_expr { Q } holds
   as well *)
let replace ?(reparse : bool = false) (node : trm) : Target.Transfo.t =
  update ~reparse (fun _t -> node)


(* [replace_fun code tg]: expects the target to point at a function call,
    then it replaces the name of the function call with the one entered
    by the user

    Assumption:
      [name] is the name of an already defined function which has the same
      signature as function whose call is targeted by [tg] *)
let replace_fun (name : string) (tg : target) : unit =
  Target.apply_on_targets (Expr_core.replace_fun name) tg
  