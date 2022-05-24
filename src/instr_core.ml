open Ast

(* [copy_aux dest_index index t]: copies instruction with [index] to the [dest_index],
     [dest_index] - the relative [index] where the target instruction will be copied,
     [index] - index of the targeted instruction,
     [delete] - if true then the original instruction will be deleted,
     [t] - ast of the surrounding sequence of the targeted instruction. *)
let copy_aux (dest_index : int) (index : int) (delete : bool) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let instr_to_copy = Mlist.nth tl index in
    let index_to_remove = if dest_index <= index then index + 1 else index in
    let new_tl = Mlist.insert_at dest_index instr_to_copy tl in
    let new_tl = if not delete then new_tl else Mlist.remove index_to_remove  1 new_tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Instr_core.copy_aux: expected the surrounding sequence of the targeted instructions"

(* [copy dest_index index delete t p]: apply [copy_aux] at trm [t] with path [p] *)
let copy (dest_index : int) (index : int) (delete : bool) : Target.Transfo.local =
  Target.apply_on_path (copy_aux dest_index index delete)

(* [accumulate_aux t]: transform a list of write instructions into a single instruction,
      [t] - the ast of the sequence containing the instructions. *)
(* LATER: Factorize me! *)
let accumulate_aux (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let nb_instr = Mlist.length tl in
    if nb_instr < 2 then fail t.loc "Instr_core.accumulate_aux: expected at least two instructions";
    let is_infix_op = ref false in
    Mlist.fold_lefti (fun i acc t1 ->
      begin match t1.desc with
      | Trm_apps (f, [ls; rs])  ->
        begin match rs.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop binop))} as f, [ls1; rs1]) ->
          if i = 0 then rs1
          else if i = nb_instr - 1
            then
              let acc = trm_apps f [acc; rs1] in
              let acc_trm = trm_apps f [ls1; acc] in
              if !is_infix_op
                then trm_pass_labels t (trm_prim_compound binop ls acc_trm)
                else trm_pass_labels t (trm_set ls acc_trm)
          else
            (trm_apps f [acc; rs1])
        | _ when is_compound_assignment t1->
           let _ = is_infix_op := true in
           begin match trm_prim_inv f with
           | Some (Prim_compound_assgn_op binop) ->
             if i = 0 then rs
             else if i = nb_instr - 1
              then
                let acc_trm = trm_apps (trm_binop binop) [acc; rs] in
                trm_pass_labels t (trm_prim_compound binop ls acc_trm)
             else
              trm_apps (trm_binop binop) [acc; rs]
           | _ -> fail t.loc "Instr_core.accumulate_aux: this should never happen"
           end
        | _-> fail t.loc "Instr_core.accumulate_aux: expected an instruction of the form x += A or x = x + A"
        end
      | _ -> fail t.loc "Instr_core.accumulate_aux: all the instructions should be write operations"
      end

    ) (trm_int 0) tl

  | _ -> fail t.loc "Instr_core.accumulate_aux: expected a block of instructions"

(* [accumulate t p]: applies [accumulate_aux] at the trm [t] with path [p]. *)
let accumulate : Target.Transfo.local =
  Target.apply_on_path (accumulate_aux)
