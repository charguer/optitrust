open Ast
open Target



(* [update f tg] applies the operation [f] to the targeted expressions *)
let update ?(reparse: bool = false)  (f : trm -> trm) : Target.Transfo.t =
  Target.reparse_after ~reparse (Target.apply_on_targets (Instr_core.update f))


(* [replace node tg] expects the target to point at an instruction,
    then it will replace this instruction with [node]. Note that [node] can be
    also some code entered as string which is transformed into a trm through function code
    then this node is merged into the ast by doing a reparse of the full ast.

   @correctness: Needs local manual reproving that if an invariant in the
   previous proof was { H } old_expr { Q } then { H } new_expr { Q } holds
   as well.
*)
let replace ?(reparse : bool = false) (node : trm) : Target.Transfo.t =
  update ~reparse (fun _t -> node)


(* [replace_fun code tg] expects the target to point to a function call,
    it then replaces the name of the function call with the one entered
    by the user

    Assumption:
      [name] is the name of an already defined function which has the same
      signature as function whose call is targeted by [tg]
*)
let replace_fun (name : string) (tg : target) : unit =
  Target.apply_on_targets (Expr_core.replace_fun name) tg
  