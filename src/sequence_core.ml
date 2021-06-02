open Ast
open Target
open Tools (* TODO: use Tools. *)


(* seq_insert_here: This function is an auxiliary function for seq_insert
    params:
      index: and integer in range 0 .. (nbinstr-1)
      ts: a list of trms which the inner sequence will contain
      subt: the trm do be modified
    return:
      the updated ast
*)
(* TODO: Sequence_core.insert_aux  and [Sequence_core.insert i [t1;t2] p] *)
let seq_insert_here (index : int) (ts : trm list) (subt : trm): trm =
    match subt.desc with
    | Trm_seq tl ->
      let tl = insert_sublist_at ts index tl in
      trm_seq ~annot:subt.annot tl
    | _ -> fail subt.loc "seq_insert: expected the sequence on which the insertion is performed"

(* seq_insert: Insert a list of instructions at the given index as a new sequence
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instructions inside the sequence)
      ts: a list of instructions(objects of type trm)
    return: the updated ast

*)
let seq_insert (path_to_seq : path) (index : int) (ts : trm list) (t : trm) : trm =
  apply_local_transformation (seq_insert_here index ts) t path_to_seq

(* seq_delete_here: This function is an auxiliary function for seq_delete
    params:
      ts: a trm list
      subt: an ast subterm
    return: the updated ast

*)
let seq_delete_here (ts : trm list) (subt : trm) : trm =
  match subt.desc with
    | Trm_seq tl ->
      (* Remove trms*)
      let tl = list_remove_set ts tl in
      (* Apply the changes *)
      trm_seq ~annot:subt.annot tl
    | _ -> fail subt.loc "seq_delete: expected the sequence on which the trms are deleted"

(* seq_delete: Remove a list of instructions at the given index as a new sequence(TODO: Ask Arthur if index is needed here)
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instructions inside the sequence)
      instr: a list of instructions(objects of type trm)
    return: the updated ast

*)
let seq_delete (path_to_seq : path) (instr : trm list) (t : trm): trm =
  apply_local_transformation(seq_delete_here instr ) t path_to_seq


(* seq_sub_here: This function is an auxiliary function for seq_sub
    params:
      index: index where the grouping is performed
      ts: a trm list
      subt: an ast subterm
    return: the updated ast

*)
(* TODO: takedrop : int -> 'a list -> 'a list * 'a list
  splits a list in two at a given index

  t0 t1 t2 t3 t4 t5 t6 t7 t8
  sub at index 2 with nb 3

  tlfront,tlrest = takedrop 2 tl
  tsub,tback = takedrop 3 trest

  tlfront = t0 t1
  tlsub = t2 t3 t4
  tlback = t5 t6 t7 t8

  trm_seq (tlfront @ (trm_seq tlsub) :: tlback)

  --
  Note: that insert i ts tl =
    let tlfront,tlback = takedrop i tl in
    tlfornt ++ ts ++ tlback

  let update i v l =   // or replace
  *)
let seq_sub_here (index : int) (ts : trm list) (subt : trm) : trm =
  match subt.desc with
    | Trm_seq tl ->
      (* First we remove this trms from the sequence *)
      let tl = list_remove_set ts tl in
      (* Create the inner sequence*)
      let sub_seq = trm_seq ts in
      (* Insert at the given index the new trm *)
      let tl = insert_in_list_at sub_seq index tl in
      (* Apply changes *)
      trm_seq ~annot:subt.annot tl
    | _ -> fail subt.loc "seq_sub: expected the sequence on which the grouping is performed"

(* seq_sub: Group the targeted instructions into one nested seq term.
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instrucitons inside the sequence)
      instr: a list of instructions(objects of type trm)
    return: the updated ast
*)
let seq_sub (path_to_seq : path) (index : int) (instr : trm list) (t : trm): trm =
  apply_local_transformation(seq_sub_here index instr ) t path_to_seq

(* seq_inline_here: This function is an auxiliary function for seq_inline
    params:
      index: index of the sequence
      subt: an ast subterm
    return: the updated ast
*)

let seq_inline_here (index : int) (subt : trm) : trm =
  match subt.desc with
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
      | _ -> fail subt.loc "seq_inline: inner sequence was not found, make sure the index is correct"
      end
      in
      (* Insert at the given index the trms from the inner sequence *)
      let tl = insert_sublist_in_list inner_seq_trms index tl in
      (*  list_insert index inner_seq (list_remove index tl)
          list_remove_and_insert_several index inner_seq tl *)
      (* Apply the changes *)
      trm_seq ~annot:subt.annot tl
    | _ -> fail subt.loc "seq_inline: expected the sequence on which the ilining is performed"


(* seq_inline: Inline the inner sequence into the outer one.
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instrucitons inside the sequence)
    return: the updated ast
*)
let seq_inline (path_to_seq : path) (index : int) (t : trm): trm =
  apply_local_transformation (seq_inline_here index ) t path_to_seq


(* seq_wrap_here: This is an auxiliary function for seq_wrap
   params:
    subt: an ast subterm
    visible: turn on(off) curly braces of the sequence
 *)
let seq_wrap_here (visible : bool) (subt : trm) : trm =
  trm_seq ~annot:(if not visible then Some No_braces else None) [subt]

(* seq_wrap: Turn the an instruction into a sequence containing only that instruction
    params:
      path_to_instr: explicit path towards the sequence
      visible: a boolean to decide if the wraped sequence should be visible or not
    return: the updated ast
*)
let seq_wrap (path_to_instr : path) (visible : bool) (t : trm): trm =
  apply_local_transformation (seq_wrap_here visible) t path_to_instr


(* seq_unrwap_here: This function is an auxiliary function for seq_unwrap
   params:
    subt: an ast subterm
   return: the updated ast
 *)
let seq_unwrap_here (subt : trm) : trm =
  match subt.desc with
    | Trm_seq tl ->
       begin match tl with
       | [el] -> el
       | _ -> fail subt.loc "seq_unwrap: can only unwrap a sequence with exactly one item"
       end
    | _ -> fail subt.loc "seq_unwrap: expected to operate on a sequence"

(* seq_unwrap: The inverse of seq_wrap , remove the sequence and replace it directly with the trms it contains
    params:
      path_to_seq: explicit path towards the sequence
      visible: a boolean to decide if the wraped sequence should be visible or not
    return: the updated ast
*)
let seq_unwrap (path_to_seq : path) (t : trm): trm =
  apply_local_transformation (seq_unwrap_here ) t path_to_seq

(* TODO: Implement later seq_distrib_ref after references have been implemented *)
