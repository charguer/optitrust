open Ast


(* [insert_if single_branch index cond t]: takes one or two instructions and create an if statement or an if else 
     statment when [single_brnach] is true,
      [cond] - condition of the if statement given as string as a trm,
      [t] - ast of the outer sequence containing the instruction. *)
let insert_if_aux (cond : trm) (mark : mark) (t : trm) : trm =
  begin match t.desc with 
  | Trm_seq _ -> trm_add_mark mark (trm_if cond t t) 
  | _ -> trm_add_mark mark (trm_if cond (trm_seq_nomarks [t]) (trm_seq_nomarks [t])) 
  end 


(* [insert_if cond makr t p]: apply [insert_if] at trm [t] with path [p]. *)
let insert_if (cond : trm) (mark : mark ): Target.Transfo.local =
  Target.apply_on_path (insert_if_aux cond mark)
