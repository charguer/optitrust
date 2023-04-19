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

(* [may_merge_ifs]: if [t] corresponds to two nested ifs, merge them using '&&'.
   *)
let may_merge_ifs (t : trm) : trm option =
  match trm_if_inv t with
  | Some (cond1, then1, else1) ->
    if is_trm_unit else1 then
    let then1' = begin match trm_seq_inv then1 with
    | Some instrs when (Mlist.length instrs) = 1 ->
      Mlist.nth instrs 0
    | _ -> then1
    end in
    begin match trm_if_inv then1' with
    | Some (cond2, then2, else2) ->
      if is_trm_unit else2 then
        Some (trm_if (trm_and cond1 cond2) then2 (trm_unit ()))
      else None
    | None -> None
    end
    else None
  | None -> None