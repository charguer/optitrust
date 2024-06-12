open Prelude
open Target


(** [update f tg]: applies the operation [f] at the target [tg] *)
let update ?(reparse: bool = false) (f : trm -> trm) (tg: target) : unit =
  Target.reparse_after ~reparse (Target.apply_at_target_paths f) tg


(** [replace node tg]: expects the target to point at an instruction, then it will replace this
    instruction with [node]. Note that [node] can be also some code entered as string if that is
    the case then to integrate it on the current ast this transformation shoudl be called with the flag ~reparse:true

   @correctness: Needs local manual reproving that if an invariant in the
   previous proof was { H } old_expr { Q } then { H } new_expr { Q } holds
   as well *)
let replace ?(reparse : bool = false) (node : trm) : target -> unit =
  update ~reparse (fun _t -> node)


(** [replace_fun code tg]: expects the target to point at a function call,
    then it replaces the name of the function call with the one entered
    by the user

    Assumption:
      [name] is the name of an already defined function which has the same
      signature as function whose call is targeted by [tg] *)
let replace_fun (name : var) (tg : target) : unit =
  Target.apply_at_target_paths (Expr_core.replace_fun_on name) tg


(** [view_subterms tg]: displays on stdout all the subterms of the targeted term.
   For viewing on stdout all subterms of a program, use:
     Expr.view_subterms [];
   which is like
     Expr.view_subterms [dRoot];
   and possibly specify a regexp to investigate:
     Expr.view_subterms ~constr:(sInstr "+= 2") [dRoot];
   Note that this is for debugging purpose only. *)
let view_subterms ?(constr:Constr.constr option) ?(rexp : Constr.rexp option) (tg : Target.target) : unit =
  let tg = if tg = [] then [dRoot] else tg in
  let ro = match constr, rexp with
    | None, None -> None
    | Some (Constr_regexp r), None -> Some r
    | Some _, None -> failwith "Expr_basic.view_subterms: [~constr] argument must be a regexp-constraint (e.g., sInstr or sExpr)"
    | None, Some r -> Some r
    | Some _, Some _ -> failwith "Expr_basic.view_subterms: cannot provide both [~constr] and [rexp]"
    in
  let stringreprs = Target.compute_stringreprs_and_update_ast (fun _ -> true) in
  (* for debug: AstC_to_c.print_stringreprs stringreprs; *)
  Target.apply_at_target_paths (Expr_core.view_subterms_on stringreprs ro) tg
