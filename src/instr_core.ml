open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which a called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [update f t]: replace an expression [t] with [f t]. *)
let update (f : trm -> trm) : Target.Transfo.local =
  Target.apply_on_path f

(* TODO: replace can be implemented as a particular case of update *)

(* [replace code t]: replace an instruction with arbitrary code or ast code
    params:
      [t]: ast node which is going to replace the current one
    return:
      updated ast with the replaced trm
 *)
let replace (t : trm) : Target.Transfo.local =
  Target.apply_on_path (fun _t ->  t)




(* [replace_fun name t]: change the current function call
      to another function call with where the function called now
      has name [name]
    params:
      [name]: name of the function replacing the targeted one
      [t]: ast of the function call trm
    return:
      updated ast with the replaced trm
 *)

let replace_fun_aux (name : string) (t : trm) : trm =
  match t.desc with
  | Trm_apps (_, args) ->
    trm_apps ~annot:t.annot ~marks:t.marks ~typ:t.typ (trm_var name) args
  | _ -> fail t.loc "replace_fun: expected a function call"


let replace_fun (name : string) : Target.Transfo.local =
  Target.apply_on_path (replace_fun_aux name)

(* [move_aux index tg_index t]: move instruction at [index] to [index_instr]
    in the sequence [t]
    params:
      [index]: index where the instr should be move to
      [tg_index]: the current index of the targeted instruction
    return:
      the updated [t]
*)
let move_aux (dest_index : int) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let instr_to_move = Mlist.nth tl index in
    let index_to_remove = if dest_index <= index then index + 1 else index in
    let new_tl = Mlist.insert_at dest_index instr_to_move tl in
    let new_tl = Mlist.remove index_to_remove  1 new_tl in
    trm_seq ~annot:t.annot ~marks:t.marks new_tl
  | _ -> fail t.loc "move_aux: expected the surrounding sequence of the targeted instructions"



let move (dest_index : int) (index : int) : Target.Transfo.local =
  Target.apply_on_path (move_aux dest_index index)

(* [accumulate_aux t] transform a list of write instructions into a single instruction
    params:
      [t]: the ast of the sequence containing the instructions
    return:
      the ast of the single write instruction where the value that is written into
      is the accumulated trm from all the initial write instructions, the operation used for
      the accumulation is the one used in each write operation
 *)

let accumulate_aux (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let nb_instr = Mlist.length tl in
    if nb_instr < 2 then fail t.loc "accumulate_aux: expected at least two instructions";
    let is_infix_op = ref false in
    Mlist.fold_lefti (fun i acc t1 ->
      begin match t1.desc with
      | Trm_apps (_, [ls; rs]) when is_set_operation t1 ->
        begin match rs.desc with
        | Trm_apps (f, [ls1; rs1]) ->
          if i = 0
            then begin
              if List.mem App_and_set t1.annot then is_infix_op := true;
            rs1
            end
          else if i = nb_instr - 1
            then
              let acc = trm_apps f [acc; rs1] in
              let acc_trm = trm_apps f [ls1; acc] in
              if !is_infix_op
                then trm_annot_add App_and_set (trm_set ls acc_trm)
                else trm_set ls acc_trm
          else
            (trm_apps f [acc; rs1])
          (* let acc = if i = 0
            then
             begin
               if List.mem App_and_set t1.annot then is_infix_op := true;
               acc
             end
            else acc in
          let acc_trm = if i = nb_instr - 1 then trm_apps f [ls1; acc] else (trm_apps f [acc; rs1]) in
          if i = nb_instr -1
            then
              if !is_infix_op
                then trm_annot_add App_and_set (trm_set ls acc_trm)
                else trm_set ls acc_trm
            else acc_trm  *)
        | _-> fail t.loc "accumulate_aux: expected an instruction of the form x += A or x = x + A"
        end
      | _ -> fail t.loc "accumulate_aux: all the instructions should be write operations"
      end

    ) (trm_int 0) tl

  | _ -> fail t.loc "accumulate_aux: expected a block of instructions"


let accumulate : Target.Transfo.local =
  Target.apply_on_path (accumulate_aux)

