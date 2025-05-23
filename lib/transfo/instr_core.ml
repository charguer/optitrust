open Prelude

(** [copy_at dest_index index t]: copies instruction at [index] to the [dest_index],
     [dest_index] - the [index] where the target instruction will be copied,
     [index] - index of the targeted instruction,
     [t] - ast of the surrounding sequence of the targeted instruction. *)
let copy_at (mark_copy : mark) (dest_index : int) (index : int) (t: trm) : trm =
  let error = "Instr_core.copy_aux: expected the surrounding sequence of the targeted instructions." in
  let tl, result = trm_inv ~error trm_seq_inv t in
  let instr_to_copy = trm_add_mark mark_copy (Mlist.nth tl index) in
  let instr_to_copy = trm_copy instr_to_copy in
  let new_tl = Mlist.insert_at dest_index instr_to_copy tl in
  trm_seq ~annot:t.annot ?result new_tl


(** [move_at dest_index index t]: moves instruction at [index] to the [dest_index],
     [dest_index] - the [index] where the target instruction will be copied,
     [index] - index of the targeted instruction,
     [t] - ast of the surrounding sequence of the targeted instruction. *)
let move_at (dest_index: int) (index: int) (t: trm): trm =
  let error = "Instr_core.copy_aux: expected the surrounding sequence of the targeted instructions." in
  let tl, result = trm_inv ~error trm_seq_inv t in
  let instr_to_copy = Mlist.nth tl index in
  let index_to_remove = if dest_index <= index then index + 1 else index in
  let new_tl = Mlist.insert_at dest_index instr_to_copy tl in
  let new_tl = Mlist.remove index_to_remove 1 new_tl in
  trm_seq ~annot:t.annot ?result new_tl


(** [accumulate_on t]: transform a list of write instructions into a single instruction,
      [t] - the ast of the sequence containing the instructions. *)
(* LATER: Factorize me! *)
let accumulate_on (t : trm) : trm =
  match t.desc with
  | Trm_seq (tl, result) ->
    if Option.is_some result then trm_fail t "Instr_core.accumulate_on: expected a sequence without return value";
    let nb_instr = Mlist.length tl in
    if nb_instr < 2 then trm_fail t "Instr_core.accumulate_on: expected at least two instructions";
    let is_infix_op = ref false in
    Mlist.fold_lefti (fun i acc t1 ->
      begin match t1.desc with
      | Trm_apps (f, [ls; rs], _, _)  ->
        begin match rs.desc with
        | Trm_apps ({desc = Trm_prim (typ, Prim_binop binop)} as f, [ls1; rs1], _, _) ->
          if i = 0 then rs1
          else if i = nb_instr - 1
            then
              let acc = trm_apps f [acc; rs1] in
              let acc_trm = trm_apps f [ls1; acc] in
              if !is_infix_op
                then trm_pass_labels t (trm_compound_assign ~typ binop ls acc_trm)
                else trm_pass_labels t (trm_set ls acc_trm)
          else
            (trm_apps f [acc; rs1])
        | _ when is_compound_assignment t1->
           let _ = is_infix_op := true in
           begin match trm_prim_inv f with
           | Some (typ, Prim_compound_assign_op binop) ->
             if i = 0 then rs
             else if i = nb_instr - 1
              then
                let acc_trm = trm_apps (trm_binop typ binop) [acc; rs] in
                trm_pass_labels t (trm_compound_assign ~typ binop ls acc_trm)
             else
              trm_apps (trm_binop typ binop) [acc; rs]
           | _ -> trm_fail t "Instr_core.accumulate_on: this should never happen"
           end
        | _-> trm_fail t "Instr_core.accumulate_on: expected an instruction of the form x += A or x = x + A"
        end
      | _ -> trm_fail t "Instr_core.accumulate_on: all the instructions should be write operations"
      end

    ) (trm_int 0) tl

  | _ -> trm_fail t "Instr_core.accumulate_on: expected a block of instructions"
