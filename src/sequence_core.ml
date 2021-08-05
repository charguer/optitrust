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
let intro_aux (label : string) (index : int) (nb : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      if nb > 0 then 
        let lfront, lback = Tools.split_list_at index tl in
        let l_intro,lback = Tools.split_list_at nb lback in
        let intro_seq = trm_seq  l_intro in
        let intro_seq = if label <> "" then trm_labelled label intro_seq else intro_seq in
          trm_seq (lfront @ [intro_seq] @ lback)
      else 
        let lfront, lback = Tools.split_list_at (index+1) tl in
        let lfront, l_intro = Tools.split_list_at (List.length lfront + nb) lfront in
        let intro_seq = trm_seq  l_intro in
        let intro_seq = if label <> "" then trm_labelled label intro_seq else intro_seq in
          trm_seq (lfront @ [intro_seq] @ lback)
    | _ -> fail t.loc "intro_aux: expected the sequence on which the grouping is performed"

let intro (label : string) (index : int) (nb_instr : int) : Target.Transfo.local =
  Target.apply_on_path (intro_aux label index nb_instr)

(*[elim_aux index t]: inline an inner sequence into an outer sequence.
    params:
      t: ast of the sequence wanted to remove
    return: 
      a hiden sequence which is going to be merged witht the outer sequence on the next step
*)
let elim_aux (t : trm) : trm =
  match t.desc with
   | Trm_labelled (_ , t1) ->
      begin match t1.desc with 
      | Trm_seq tl1 -> 
        trm_seq_no_brace tl1
      | _ -> fail t.loc "elim_aux: expected a sequence of terms"
     end
   | Trm_seq tl ->
      trm_seq_no_brace tl
   | _ -> fail t.loc "elim_aux: expected the sequence to be deleteds"


let elim : Target.Transfo.local =
  Target.apply_on_path (elim_aux)

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

(* [split_aux index t ]: splitting a sequence in two parts 
    params:
      index: index of the realative target entered by the user
      t : an ast term that corresponds to the the targeted sequence
    return:
      the updated ast with the splitted sequence
*)
let split_aux (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let first_part, last_part = Tools.split_list_at index tl in
    trm_seq_no_brace [trm_seq ~annot:t.annot first_part;trm_seq ~annot:t.annot last_part]
  | _ -> fail t.loc "split_aux: expected a sequence, containing the location where it is going to be splitted"

let split (index : int) : Target.Transfo.local =
  Target.apply_on_path (split_aux index)


let partition_aux (blocks : int list) (visible : bool) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl -> 
    let sum_blocks = List.fold_left (+) 0 blocks in
    if sum_blocks <> List.length tl 
      then fail t.loc "partition: the partition entered is not correct"
      else
        let current_list = ref tl in
        let partition = List.fold_left (fun acc x -> 
            let lfront, lback = Tools.split_list_at x !current_list in
            current_list := lback;
            lfront :: acc
        ) [] blocks in
        begin match visible with 
        | true -> trm_seq ~annot:t.annot (List.map (trm_seq) (List.rev partition))
        | false -> trm_seq ~annot:t.annot (List.map (trm_seq_no_brace) (List.rev partition))
        end
        
  | _ -> fail t.loc "partial_aux: expected a sequence to partition"

let partition (blocks : int list) (visible : bool): Target.Transfo.local =
  Target.apply_on_path (partition_aux blocks visible)