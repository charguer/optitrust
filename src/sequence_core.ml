open Ast
(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [insert_aux index ts t]: insert an arbitrary trm after or before a targeteted trm
    params:
      index: and integer in range 0 .. (nbinstr-1)
      ts: a list of trms which the inner sequence will contain
      t: a term that corresponds to the outer sequence.
    return: the updated sequence where the outer sequence with the augmented new trm.
*)
let insert_aux (index : int) (s : string) (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
      let lfront, lback = Tools.split_list_at index tl in
      let new_trm = trm_seq ~annot:(Some No_braces) [trm_arbitrary s] in
      trm_seq ~annot:t.annot  (lfront @ [new_trm] @ lback)
    | _ -> fail t.loc "insert_aux: expected the sequence on which the insertion is performed"

let insert (index : int) (s : string) : Target.Transfo.local =
  Target.apply_on_path (insert_aux index s)

(* [delete_aux index nb_instr t]: delete a number of instruction inside the sequence starting 
                                  from index [index] and ending at ([index] + [nb])
    params:
      nb: number of instructions to delete
      t: a term that corresponds to the outer sequence.
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


(* [sub_aux index nb t]: inside a sequence, move all the trms with findex falling in a rance 
                         from [index] to [index] + [nb] into a sub-sequence.
    params:
      index: index where the grouping is performed
      ts: a trm list
      t: a term that corresponds to the outer sequence.
    return: the updated ast

*)
let sub_aux (label : string) (index : int) (nb : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      let lfront, lback = Tools.split_list_at index tl in
      let l_sub,lback = Tools.split_list_at nb lback in
      let sub_seq = trm_seq  l_sub in
      let sub_seq = if label <> "" then trm_labelled label sub_seq else sub_seq in
        trm_seq (lfront @ [sub_seq] @ lback)
    | _ -> fail t.loc "sub_aux: expected the sequence on which the grouping is performed"


let sub (label : string) (index : int) (nb_instr : int) : Target.Transfo.local =
  Target.apply_on_path (sub_aux label index nb_instr)


(* [sub_between_aux index1 index2 t]: inside a sequence get the index of two trms, then move all the 
                                      trms with index falling in the range of the two trms into a sub-sequence
    params:
      index1: index where the grouping starts
      index2: index where the grouping ends
      t: a term that corresponds to the outer sequence.
    return: the updated ast

*)
let sub_between_aux (label : string) (index1 : int) (index2 : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      let lfront, lback = Tools.split_list_at index2 tl in
      let lfront, l_sub = Tools.split_list_at index1 lfront in
      let sub_seq = trm_seq  l_sub in
      let sub_seq = if label <> "" then trm_labelled label sub_seq else sub_seq in
        trm_seq  (lfront @ [sub_seq] @ lback)
    | _ -> fail t.loc "sub_aux: expected the sequence on which the grouping is performed"


let sub_between (label : string) (index1 : int) (index2 : int) : Target.Transfo.local =
  Target.apply_on_path (sub_aux label index1 index2)


(*[inline_aux index t]: inline an inner sequence into an outer sequence.
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
      let inner_seq = begin match inner_seq with
        | [ins] -> ins
        | _ -> fail t.loc "inline_aux: exected a list with only one element"
        end in
      let inner_seq_trms = 
        begin match inner_seq.desc with
        | Trm_seq tl1 -> tl1
        | _ -> fail t.loc "inline_aux: inner sequence was not found, make sure the index is correct"
        end in
      trm_seq ~annot:t.annot (lfront @ inner_seq_trms @ lback)
    | _ -> fail t.loc "inline_aux: expected the sequence on which the ilining is performed"


let inline (index : int) : Target.Transfo.local =
  Target.apply_on_path (inline_aux index)


(* [wrap_aux visible label t]: replacing t with a sequence that contains t as single item.
   params:
    t: any term
    visible: a flag to turn on(off) curly braces of the sequence
   return: the outer sequence containing t
 *)
let wrap_aux (visible : bool) (label : string) (t : trm) : trm =
  let wrapped_seq = if visible then trm_seq [t] else trm_seq ~annot:(Some No_braces) [t] in
  if label <> "" then trm_labelled label wrapped_seq else wrapped_seq 
 
let wrap (visible : bool) (label : string) : Target.Transfo.local=
  Target.apply_on_path (wrap_aux visible label)


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

let unwrap : Target.Transfo.local =
  Target.apply_on_path (unwrap_aux)
