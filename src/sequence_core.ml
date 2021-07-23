open Ast
(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [insert_aux index ts t]: insert an arbitrary trm after or before a targeted trm
    params:
      index: and integer in range 0 .. (nbinstr-1)
      ts: a list of trms which the inner sequence will contain
      t: ast of the outer sequence where the insertion will be performed.
    return: 
      the updated ast of the updated outer sequence with the augmented new trm
*)
let insert_aux (index : int) (s : string) (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
      let lfront, lback = Tools.split_list_at index tl in
      (* let new_trm = trm_seq_no_brace [trm_arbitrary s] in *)
      let new_trm = trm_arbitrary s in
      trm_seq ~annot:t.annot  (lfront @ [new_trm] @ lback)
    | _ -> fail t.loc "insert_aux: expected the sequence on which the insertion is performed"

let insert (index : int) (s : string) : Target.Transfo.local =
  Target.apply_on_path (insert_aux index s)

(* [delete_aux index nb_instr t]: delete a number of instruction inside the sequence starting 
      from index [index] and ending at ([index] + [nb])
    params:
      nb: number of instructions to delete
      t: ast of the outer sequence where the deletion is performed.
    return: 
      the updated ast of the surrounding sequence with the deleted nodes
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


(* [intro_aux index nb t]: inside a sequence, move all the trms with findex falling in a rance 
      from [index] to [index] + [nb] into a sub-sequence.
    params:
      index: index where the grouping is performed
      ts: a list of ast nodes
      t: ast of the outer sequence where the insertion is performed
    return: the updated ast of surrounding sequence with the inserted nodes

*)
let intro_aux (label : string) (visible : bool) (index : int) (nb : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      let lfront, lback = Tools.split_list_at index tl in
      let l_intro,lback = Tools.split_list_at nb lback in
      let intro_seq = trm_seq  l_intro in
      let intro_seq = if label <> "" then trm_labelled label intro_seq else intro_seq in
        if visible then trm_seq (lfront @ [intro_seq] @ lback)
          else trm_seq (lfront @ [intro_seq] @ lback)
    | _ -> fail t.loc "intro_aux: expected the sequence on which the grouping is performed"

let intro (label : string) (visible : bool) (index : int) (nb_instr : int) : Target.Transfo.local =
  Target.apply_on_path (intro_aux label visible index nb_instr)

(*[elim_aux index t]: inline an inner sequence into an outer sequence.
    params:
      index: a valid index in the outer sequence; at that index, the subterm
         should correspond to the inner sequence
      t: ast of the outer sequence where inlining is done
    return: 
      updated ast of the outer sequence, where the elements from the inner
      sequence are directly laid out there.
*)
let elim_aux (index : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      let lfront, lback = Tools.split_list_at index tl in
      let inner_seq, lback = Tools.split_list_at 1 lback in
      let inner_seq = begin match inner_seq with
        | [ins] -> ins
        | _ -> fail t.loc "elim_aux: exected a list with only one element"
        end in
      let inner_seq_trms = 
        begin match inner_seq.desc with
        | Trm_seq tl1 -> tl1
        | _ -> fail t.loc "elim_aux: inner sequence was not found, make sure the index is correct"
        end in
      trm_seq ~annot:t.annot (lfront @ inner_seq_trms @ lback)
    | _ -> fail t.loc "elim_aux: expected the sequence on which the ilining is performed"


let elim (index : int) : Target.Transfo.local =
  Target.apply_on_path (elim_aux index)

(* [intro_on_instr_aux visible label t]: replacing t with a sequence that contains t as single item.
   params:
    label: add a label around the sequence
    visible: a flag to turn on(off) curly braces of the sequence
    t: ast of the instruction 
   return: 
    updated ast of the outer sequence with wrapped node t
 *)
let intro_on_instr_aux (label : string) (visible : bool) (t : trm) : trm =
  let wrapped_seq = if visible then trm_seq [t] else trm_seq_no_brace [t] in
  if label <> "" then trm_labelled label wrapped_seq else wrapped_seq 
 
let intro_on_instr (visible : bool) (label : string) : Target.Transfo.local=
  Target.apply_on_path (intro_on_instr_aux label visible)

(* [unrwap_aux t]: replacing a sequence that contains a single item t with t.
   params:
    t: a term that corresponds to a sequence with a single item in t
   return:
    the udated the ast where the trm inside the sequence has been extracted
 *)
let unwrap_aux (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
       begin match tl with
       | [el] -> el
       | _ -> fail t.loc "unwrap_aux: can only unwrap a sequence with exactly one item"
       end
    | _ -> fail t.loc "unwrap_aux: expected to operate on a sequence"

let unwrap : Target.Transfo.local =
  Target.apply_on_path (unwrap_aux)
