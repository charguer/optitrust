open Ast
open Target
open Tools (* TODO: use Tools. *)


(* insert_aux: This function is an auxiliary function for insert
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
      let tl = insert_sublist_at ts index tl in
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "insert_aux: expected the sequence on which the insertion is performed"

(* insert: Insert a list of instructions at the given index as a new sequence
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instructions inside the sequence)
      ts: a list of instructions(objects of type trm)
    return: the updated ast

*)
let insert (index : int) (ts : trm list) (path_to_seq : path) (t : trm) : trm =
  Target.apply_on_path (insert_aux index ts) t path_to_seq

(* delete_aux: This function is an auxiliary function for delete
    params:
      nb: number of instructions to delete
      t: an ast subterm
    return: the updated ast

*)
let delete_aux (index : int) (nb_instr : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      let lfront,lback = split_list_at index tl in
      let _,lback = split_list_at (index + nb_instr) lback in
      (* Remove trms*)
      let tl = lfront @ lback in
      (* Apply the changes *)
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "delete_aux: expected the sequence on which the trms are deleted"

let delete (index : int) (nb_instr : int) : Target.Transfo.local=
  Target.apply_on_path(delete_aux index nb_instr)


(* sub_aux: This function is an auxiliary function for sub
    params:
      index: index where the grouping is performed
      ts: a trm list
      t: an ast subterm
    return: the updated ast

*)

let sub_aux (index : int) (nb : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      let lfront,lback = split_list_at index tl in
      let l_sub,lback = split_list_at (index + nb) lback in
      (* Create the inner sequence*)
      let sub_seq = trm_seq l_sub in
      let tl = lfront @ [sub_seq] @ lback in
      
      (* Apply changes *)
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "sub_aux: expected the sequence on which the grouping is performed"

(* sub: Group the targeted instructions into one nested seq term.
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instrucitons inside the sequence)
      instr: a list of instructions(objects of type trm)
    return: the updated ast
*)
let sub (index : int) (nb_instr : int)  =
  Target.apply_on_path(sub_aux index nb_instr) 

(* inline_aux: This function is an auxiliary function for inline
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
      let tl = insert_sublist_in_list inner_seq_trms index tl in
      (*  list_insert index inner_seq (list_remove index tl)
          list_remove_and_insert_several index inner_seq tl *)
      (* Apply the changes *)
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "inline_aux: expected the sequence on which the ilining is performed"


(* inline: Inline the inner sequence into the outer one.
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instrucitons inside the sequence)
    return: the updated ast
*)
let inline (index : int) : Target.Transfo.local =
  Target.apply_on_path (inline_aux index)


(* wrap_aux: This is an auxiliary function for wrap
   params:
    t: an ast subterm
    visible: turn on(off) curly braces of the sequence
 *)
let wrap_aux (visible : bool) (t : trm) : trm =
  trm_seq ~annot:(if not visible then Some No_braces else None) [t]

(* wrap: Turn the an instruction into a sequence containing only that instruction
    params:
      path_to_instr: explicit path towards the sequence
      visible: a boolean to decide if the wraped sequence should be visible or not
    return: the updated ast
*)
let wrap (visible : bool) : Target.Transfo.local=
  Target.apply_on_path (wrap_aux visible)


(* unrwap_aux: This function is an auxiliary function for unwrap
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

(* unwrap: The inverse of wrap , remove the sequence and replace it directly with the trms it contains
    params:
      path_to_seq: explicit path towards the sequence
      visible: a boolean to decide if the wraped sequence should be visible or not
    return: the updated ast
*)
let unwrap : Target.Transfo.local =
  Target.apply_on_path (unwrap_aux) 

(* TODO: Implement later distrib_ref after references have been implemented *)
