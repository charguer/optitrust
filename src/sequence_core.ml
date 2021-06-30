open Ast

(* [insert_aux index ts t]: This function is an auxiliary function for insert
    params:
      index: and integer in range 0 .. (nbinstr-1)
      ts: a list of trms which the inner sequence will contain
      t: the trm do be modified
    return:
      the updated ast
*)

let insert_aux (index : int) (s : string) (t : trm): trm =
    match t.desc with
    | Trm_seq tl ->
      let lfront, lback = Tools.split_list_at index tl in
      (* let context = Generic_core.get_context ctx (trm_seq ~annot:(Some No_braces) lfront) in *)
      (* let ts = Generic_core.stats ~context ctx s in *)

      let new_trm = trm_seq ~annot:(Some No_braces) [trm_arbitray s] in
      trm_seq ~annot:t.annot  (lfront @ [new_trm] @ lback)
    | _ -> fail t.loc "insert_aux: expected the sequence on which the insertion is performed"

(* [insert index ts path_to_seq t] *)
let insert (index : int) (s : string) : Target.Transfo.local =
  Target.apply_on_path (insert_aux  index s) 

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
      let _,lback = Tools.split_list_at nb_instr lback in
      let tl = lfront @ lback in
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "delete_aux: expected the sequence on which the trms are deleted"

(* [delete index nb_instr t p] *)
let delete (index : int) (nb_instr : int) : Target.Transfo.local =
  Target.apply_on_path (delete_aux index nb_instr)


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
      let sub_seq = trm_seq  l_sub in
      let tl = lfront @ [sub_seq] @ lback in
      trm_seq  tl
    | _ -> fail t.loc "sub_aux: expected the sequence on which the grouping is performed"


(* [sub index nb_instr] *)
let sub (index : int) (nb_instr : int) : Target.Transfo.local =
  Target.apply_on_path (sub_aux index nb_instr)


(* [sub_between_aux index1 index2 t]: This function is an auxiliary function for sub_between
    params:
      index1: index where the grouping starts
      index2: index where the grouping ends
      t: an ast subterm
    return: the updated ast

*)
let sub_between_aux (index1 : int) (index2 : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      let lfront, lback = Tools.split_list_at index2 tl in
      let lfront, l_sub = Tools.split_list_at index1 lfront in
      (* Create the inner sequence*)
      let sub_seq = trm_seq  l_sub in
      let tl = lfront @ [sub_seq] @ lback in
      (* Apply changes *)
      trm_seq  tl
    | _ -> fail t.loc "sub_aux: expected the sequence on which the grouping is performed"


(* [sub index nb_instr] *)
let sub_between (index1 : int) (index2 : int) : Target.Transfo.local =
  Target.apply_on_path (sub_aux index1 index2)


(* [inline_aux index t] expects the term [t] to point to a sequence
   such that at index [index] there is a nested sequence. It then "inlines"
   the contents of the nested sequence in the outer one.

   ALTERNATIVE
   [inline_aux index t]: inline an inner sequence into an outer sequence.
    params:
      index: a valid index in the outer sequence; at that index, the subterm
         should correspond to the inner sequence
      t: a term that corresponds to the outer sequence.
    return: the updated outer sequence, where the elements from the inner
     sequence are directly laid out there.
*)
let inline_aux (index : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      let lfront, lback = Tools.split_list_at index tl in
      let inner_seq, lback = Tools.split_list_at 1 lback in
      let inner_seq = List.hd inner_seq in
      let inner_seq_trms = begin match inner_seq.desc with
      | Trm_seq tl1 -> tl1
      | _ -> fail t.loc "inline_aux: inner sequence was not found, make sure the index is correct"
      end
      in
      trm_seq ~annot:t.annot (lfront @ inner_seq_trms @lback)
    | _ -> fail t.loc "inline_aux: expected the sequence on which the ilining is performed"

(* NOTE: redundant documentation.
   [inline index t p] expects the path [p] in the term [t] to point to a sequence
   such that at index [index] there is a nested sequence. It then "inlines" the
   contents of the nested sequence in the outer one. *)
let inline (index : int) : Target.Transfo.local =
  Target.apply_on_path (inline_aux index)


(* [wrap_aux vosobme t]: replacing t with a sequence that contains t as single item.
   params:
    t: any term
    visible: a flag to turn on(off) curly braces of the sequence
   return: the outer sequence containing t
 *)
let wrap_aux (visible : bool) (t : trm) : trm =
  trm_seq ~annot:(if not visible then Some No_braces else None) [t]

(* [wrap visible t p] *)
let wrap (visible : bool) : Target.Transfo.local=
  Target.apply_on_path (wrap_aux visible)


(* [unrwap_aux t]: replacing a sequence that contains a single item t with t.
   params:
    t: a term that corresponds to a sequence with a single item in t
   return: the sole term inside the sequence.
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


