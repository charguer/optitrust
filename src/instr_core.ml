open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which a called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [update f t]: replace an expression [t] with [f t]. *)
let update (f : trm -> trm) : Target.Transfo.local =
  Target.apply_on_path f

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
(* LATER: Factorize me! *)
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
        | _-> fail t.loc "accumulate_aux: expected an instruction of the form x += A or x = x + A"
        end
      | _ -> fail t.loc "accumulate_aux: all the instructions should be write operations"
      end

    ) (trm_int 0) tl

  | _ -> fail t.loc "accumulate_aux: expected a block of instructions"








let accumulate_aux1 (t : trm) : trm =
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
        | _-> fail t.loc "accumulate_aux: expected an instruction of the form x += A or x = x + A"
        end
      | _ -> fail t.loc "accumulate_aux: all the instructions should be write operations"
      end

    ) (trm_int 0) tl

  | _ -> fail t.loc "accumulate_aux: expected a block of instructions"


let accumulate : Target.Transfo.local =
  Target.apply_on_path (accumulate_aux)



let view_subterms_aux (ro : Constr.rexp option) (t : trm) : trm =
  let sprintf = Printf.sprintf in
  let rec aux t =
    let sloc =
      match t.loc with
      | None -> "<noloc>"
      | Some {loc_file = _filename; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}} ->
        (* Format:  is 18(4-7)  or 18(4)-20(7)  for multilines *)
        if start_row = end_row
          then sprintf "%d(%d-%d)" start_row start_column end_column
          else sprintf "%d(%d)-%d(%d)" start_row start_column end_row end_column
      in
    let strm =
      match Ast.trm_get_string_repr t with
      | Some s -> s
      | None -> "<no_string_repr>"
      in
    let styp =
      match t.typ with
      | Some ty -> AstC_to_c.typ_to_string ty
      | None -> "<notyp>"
      in
    let styp_trimmed =
      (* If styp > 15 chars, cut the end *)
      let sz = 15 in
      let s = if String.length styp <= sz then styp else (String.sub strm 0 (sz-3)) ^ "..." in
      s ^ String.make (sz - (String.length s)) ' '
      in
    let strm_trimmed =
      (* If strm > 50 chars, keep only 30 first and 20 last. and make it a single line *)
      let n = String.length strm in
      let s =
        if n <= 50
          then strm
          else (String.sub strm 0 30) ^ " ... " ^ (String.sub strm (n-20) 20)  (* ARTHUR: use a wrapper function *)
        in
      Str.global_replace (Str.regexp "\n") " " s
      in
    let tkind = Constr.get_trm_kind t in
    let skind = Constr.trm_kind_to_string tkind in
    let sreg =
      match ro with
      | None -> ""
      | Some r ->
        (if r.rexp_trm_kind <> tkind then
          sprintf "-"
        else if Constr.match_regexp_str r strm then
          sprintf "1"
        else
          sprintf "0")
      in
    let spacing1 =
      let nloc = String.length sloc in
      if nloc > 15 then "" else String.make (15 - nloc) ' ' in  (* ARTHUR: use a wrapper function *)
    let spacing2=
      let nkind = String.length skind in
      if nkind > 5 then "" else String.make (5 - nkind) ' ' in (* ARTHUR: use a wrapper function *)
    Printf.printf "%s%s: %s : %s%s : %s : %s\n" sloc spacing1 sreg skind spacing2 styp_trimmed strm_trimmed;
    trm_map aux t
    in
  aux t


let view_subterms (ro : Constr.rexp option) : Target.Transfo.local =
  Target.apply_on_path (view_subterms_aux ro)
