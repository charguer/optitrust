open Ast
open Target


(* [insert_aux index ts t]: This function is an auxiliary function for insert
    params:
      index: and integer in range 0 .. (nbinstr-1)
      ts: a list of trms which the inner sequence will contain
      t: the trm do be modified
    return:
      the updated ast
*)
(* TODO: Sequence_core.insert_aux  and [Sequence_core.insert i [t1;t2] p] *)
let insert_aux (index : int) (ts : trm list) (t : trm): trm =
    match t.desc with
    | Trm_seq tl ->
      let tl = Tools.insert_sublist_at ts index tl in
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "insert_aux: expected the sequence on which the insertion is performed"

(* [insert index ts path_to_seq t] *)
let insert (index : int) (ts : trm list) (path_to_seq : path) (t : trm) : trm =
  Target.apply_on_path (insert_aux index ts) t path_to_seq


(* [delete_aux index nb_instr t]: This function is an auxiliary function for delete
    params:
      nb: number of instructions to delete
      t: an ast subterm
    return: the updated ast

*)
let delete_aux (index : int) (nb_instr : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      let lfront,lback = Tools.split_list_at index tl in
      let _,lback = Tools.split_list_at (index + nb_instr) lback in
      (* Remove trms*)
      let tl = lfront @ lback in
      (* Apply the changes *)
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "delete_aux: expected the sequence on which the trms are deleted"

(* [delete index nb_instr t p] *)
let delete (index : int) (nb_instr : int) : Target.Transfo.local=
  Target.apply_on_path(delete_aux index nb_instr)


(* [sub_aux index nb t]: This function is an auxiliary function for sub
    params:
      index: index where the grouping is performed
      ts: a trm list
      t: an ast subterm
    return: the updated ast

*)
let sub_aux (index : int) (nb : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      let lfront,lrest = Tools.split_list_at index tl in
      let l_sub,lback = Tools.split_list_at nb lrest in
      (* Create the inner sequence*)
      let sub_seq = trm_seq l_sub in
      let tl = lfront @ [sub_seq] @ lback in
      (* Apply changes *)
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "sub_aux: expected the sequence on which the grouping is performed"


(* [sub index nb_instr] *)
let sub (index : int) (nb_instr : int)  =
  Target.apply_on_path(sub_aux index nb_instr)


(* [inline_aux index t]: This function is an auxiliary function for inline
    params:
      index: index of the sequence
      t: an ast subterm
    return: the updated ast
*)
let inline_aux (index : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
    (* TODO:
       let front,rest = takedrop index
       match rest with
       | (Trm_set tl)::back -> trm_seq (front ++ tl ++ back)
    *)
      (* Get the trms from the inner sequence *)
      let inner_seq = List.nth tl index in
      let inner_seq_trms = begin match inner_seq.desc with
      | Trm_seq tl1 -> tl1
      | _ -> fail t.loc "inline_aux: inner sequence was not found, make sure the index is correct"
      end
      in
      (* Insert at the given index the trms from the inner sequence *)
      let tl = Tools.insert_sublist_in_list inner_seq_trms index tl in
      (*  list_insert index inner_seq (list_remove index tl)
          list_remove_and_insert_several index inner_seq tl *)
      (* Apply the changes *)
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "inline_aux: expected the sequence on which the ilining is performed"


(* [inline index t p] *)
let inline (index : int) : Target.Transfo.local =
  Target.apply_on_path (inline_aux index)


(* [wrap_aux vosobme t]: This is an auxiliary function for wrap
   params:
    t: an ast subterm
    visible: turn on(off) curly braces of the sequence
 *)
let wrap_aux (visible : bool) (t : trm) : trm =
  trm_seq ~annot:(if not visible then Some No_braces else None) [t]

(*  [wrao visible t p] *)
let wrap (visible : bool) : Target.Transfo.local=
  Target.apply_on_path (wrap_aux visible)


(* [unrwap_aux t]: This function is an auxiliary function for unwrap
   params:
    t: an ast subterm
   return: the updated ast
 *)
let unwrap_aux (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
       begin match tl with
       | [el] -> el
       | _ -> fail t.loc "unwrap_aux: can only unwrap a sequence with exactly one item"
       end
    | _ -> fail t.loc "unwrap_aux: expected to operate on a sequence"


(* [unwrap t p] *)
let unwrap : Target.Transfo.local =
  Target.apply_on_path (unwrap_aux)

(* TODO: Implement later distrib_ref after references have been implemented *)
