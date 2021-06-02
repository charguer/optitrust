open Ast
open Target
open Generic
open Tools 


(* seq_insert_here: This function is an auxiliary function for seq_insert
    params: 
      index: and integer in range 0 .. (nbinstr-1)
      ts: a list of trms which the inner sequence will contain
      subt: the trm do be modified
    return:
      the updated ast 
*)
let seq_insert_here (index : int) (ts : trm list) (subt : trm): trm =
    match subt.desc with
    | Trm_seq tl ->
      (* Insert the new sequence with the given instrucitons at index int *)
      let tl = insert_sublist_at ts index tl in 
      (* Apply the changes *)
      trm_seq ~annot:subt.annot  tl
    | _ -> fail subt.loc "seq_insert: expected the sequence on which the insertion is performed"

(* seq_insert: Insert a list of instructions at the given index as a new sequence
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instructions inside the sequence) 
      ts: a list of instructions(objects of type trm)
    return: the updated ast 

*)
let seq_insert (path_to_seq : path) (index : int) (ts : trm list) (t : trm) : trm = 
  apply_local_transformation(seq_insert_here index ts ) t path_to_seq

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
      (* Get the trms from the inner sequence *)
      let inner_seq = List.nth tl index in 
      let inner_seq_trms = begin match inner_seq.desc with 
      | Trm_seq tl -> tl 
      | _ -> fail subt.loc "seq_inline: inner sequence was not found, make sure the index is correct"
      end
      in 
      (* Insert at the given index the trms from the inner sequence *)
      let tl = insert_sublist_in_list inner_seq_trms index tl in 
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
    | Trm_seq [el] -> el
    | _ -> fail subt.loc "seq_unwrap: expected the sequence wanted to remove, the sequence shoudl contain only one trm"

(* seq_unwrap: The inverse of seq_wrap , remove the sequence and replace it directly with the trms it contains
    params:
      path_to_seq: explicit path towards the sequence
      visible: a boolean to decide if the wraped sequence should be visible or not 
    return: the updated ast 
*)
let seq_unwrap (path_to_seq : path) (t : trm): trm =
  apply_local_transformation (seq_unwrap_here ) t path_to_seq    

(* TODO: Implement later seq_distrib_ref after references have been implemented *)
