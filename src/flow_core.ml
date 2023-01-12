open Ast


(* [insert_if single_branch index cond t]: takes one or two instructions and create an if statement or an if else
     statment when [single_brnach] is true,
      [cond] - condition of the if statement given as string as a trm,
      [t] - ast of the outer sequence containing the instruction. *)
let insert_if_aux (cond : trm) (mark : mark) (no_else : bool) (t : trm) : trm =
  let else_br = if no_else then trm_unit() else t in 
  if is_trm_seq t 
    then trm_add_mark mark (trm_if cond t else_br) 
    else trm_add_mark mark (trm_if cond (trm_seq_nomarks [t]) (if no_else then else_br else trm_seq_nomarks [else_br]))

(* [insert_if cond makr t p]: apply [insert_if] at trm [t] with path [p]. *)
let insert_if (cond : trm) (mark : mark ) (no_else : bool) : Target.Transfo.local =
  Target.apply_on_path (insert_if_aux cond mark no_else)
