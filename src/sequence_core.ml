open Ast

(* [insert_aux index code t]: inserts trm [code] at index [index] in sequence [t],
    [index] - a valid index where the instruction can be added,
    [code] - instruction to be added as an arbitrary trm,
    [t] - ast of the outer sequence where the insertion will be performed. *)
let insert_aux (index : int) (code : trm) (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
      let new_tl = Mlist.insert_at index code tl in
      trm_seq ~annot:t.annot ~marks:t.marks new_tl
    | _ -> fail t.loc "Sequence_core.insert_aux: expected the sequence on where insertion is performed"

(* [insert index code t p]: applies [insert_aux] at trm [t] with path [p]. *)
let insert (index : int) (code : trm) : Target.Transfo.local =
  Target.apply_on_path (insert_aux index code)

(* [delete_aux index nb_instr t]: deletes a number of instructions inside the sequence starting 
      from [index] and ending at ([index] + [nb]),
      [index] - starting index,
      [nb] - number of instructions to delete,
      [t] - ast of the outer sequence where the deletion is performed. *)
let delete_aux (index : int) (nb_instr : int) (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      trm_seq ~annot:t.annot ~marks:t.marks (Mlist.remove index nb_instr tl)
    | _ -> fail t.loc "Sequence_core.delete_aux: expected the sequence on which the trms are deleted"

(* [delete index nb_instr t p]: applies [delete_aux] at trm [t] with path [p].*)
let delete (index : int) (nb_instr : int) : Target.Transfo.local =
  Target.apply_on_path (delete_aux index nb_instr)

(* [intro_aux index nb t]: regroups instructions with indices falling in the range [index, index + nb) into a sub-sequence,
       [mark] - mark to insert on the new sub-sequence,
       [label] - a label to insert on the new sub-sequence,
       [index] - index where the grouping is performed,
       [nb] - number of instructions to consider,
       [t] - ast of the outer sequence where the insertion is performed.
    
    Note: if both the mark and the label are given then transformation will fail. *)
let intro_aux (mark : string) (label : label) (index : int) (nb : int) (t : trm) : trm =
  if mark <> "" && label <> "" then fail t.loc "intro_aux: can't insert both the label and the mark at the same time";
  match t.desc with
    | Trm_seq tl ->
      let tl1, tl2 = 
        if nb > 0 then Mlist.extract index nb tl else Mlist.extract (index+ nb+1) (-nb) tl in
        let intro_seq = trm_seq tl2 in
        let intro_seq = if mark <> "" 
                          then trm_add_mark mark intro_seq 
                          else if label <> "" then trm_labelled label intro_seq 
                          else intro_seq in
        let index = if nb < 0 then (index + nb + 1) else index in
         trm_seq  ~annot:t.annot ~marks:t.marks (Mlist.insert_at index intro_seq tl1)
    | _ -> fail t.loc "Sequence_core.intro_aux: expected the sequence on which the grouping is performed"

(* [intro mark label index nb t p]: applies [intro_aux] at trm [t] with path [p]. *)
let intro (mark : string) (label : label) (index : int) (nb : int) : Target.Transfo.local =
  Target.apply_on_path (intro_aux mark label index nb)

(* [elim_aux index t]: inlines an inner sequence into the outer one,
      [t] - ast of the sequence to be removed.  *)
let elim_aux (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
     trm_seq_no_brace (Mlist.to_list tl)
  | _ -> fail t.loc "Sequenc_core.elim_aux: expected the sequence to be deleteds"

(* [elim t p]: applies [elim_aux] at trm [t] with path [p. *)
let elim : Target.Transfo.local =
  Target.apply_on_path(Internal.apply_on_path_targeting_a_sequence (elim_aux) ~keep_label:false "elim")

(* [intro_on_instr_aux visible mark t]: surround [t] with a sequence,
    [mark] - mark to be added on the introduced sequence,
    [visible] - a flag on the visibility of the introduced sequence, 
    [t] - any trm. *)
let intro_on_instr_aux (mark : mark) (visible : bool) (t : trm) : trm =
  let wrapped_seq = if visible then trm_seq (Mlist.of_list [t]) else trm_seq_no_brace [t] in
  trm_add_mark mark wrapped_seq 
 
(* [intro_on_instr visible mark t p]: applies [intro_on_instr_aux] at trm [t] with path [p]. *)
let intro_on_instr (visible : bool) (mark : mark) : Target.Transfo.local=
  Target.apply_on_path (intro_on_instr_aux mark visible)

(* [unwrap_aux t]: the opposite of [intro_on_instr_aux] 
     [t] - a term that corresponds to a sequence with a single item in t. *)
let unwrap_aux (t : trm) : trm =
  match t.desc with
    | Trm_seq tl ->
      if Mlist.length tl = 1 then Mlist.nth tl 0 
        else fail t.loc "Sequence_core.unwrap_aux: can only unwrap a sequence with exactly one item"
    | _ -> fail t.loc "Sequence_core.unwrap_aux: expected to operate on a sequence"

(* [unwrap t p]: applies [unwrap_aux] at trm [t] with path [p]. *)
let unwrap : Target.Transfo.local =
  Target.apply_on_path (unwrap_aux)

(* [split_aux index t ]: splitts [t] into two sequences,
      [index] - the location where the splitting is done,
      [is_fun_body] - flag used when splitting function bodies,
      [t] - trm that corresponds to the the targeted sequence. *)
let split_aux (index : int) (is_fun_body : bool) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let first_part,last_part = Mlist.split index tl in
    let res = 
    trm_seq_no_brace [trm_seq first_part; trm_seq last_part] in 
    if is_fun_body then trm_seq ~annot:t.annot ~marks:t.marks (Mlist.of_list [res]) else res
  | _ -> fail t.loc "Sequence_core.split_aux: expected a sequence, containing the location where it is going to be splitted"

(* [split index is_fun_body t p]: applies [split_aux] at trm [t] with path [p]. *)
let split (index : int) (is_fun_body : bool) : Target.Transfo.local =
  Target.apply_on_path (split_aux index is_fun_body)

(* [partition blocks braces]: partitions sequence [t] into a list of sequences,
      [blocks] -  a list of integers, where each integer denotes the size of the partition blocks,
      [braces] - flag on the visibility of the generated sequences,
      [t]: trm corresponding to a sequence. *)
let partition_aux (blocks : int list) (braces : bool) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl -> 
    let nb = Mlist.length tl in
    let blocks = if blocks = [] then [nb] else blocks in
    let sum_blocks = List.fold_left (+) 0 blocks in
    if sum_blocks <> nb 
      then fail t.loc (Printf.sprintf "Sequence_core.partition: the partition entered is not correct, 
                the list length is %d, while the sum of the block size is %d" (Mlist.length tl) sum_blocks)
      else
        let current_list = ref tl in
        let partition = List.fold_left (fun acc x -> 
          let lfront, lback = Mlist.split x !current_list in
          current_list := lback;
          lfront :: acc
          ) [] blocks 
          in
        let new_tl = 
          if braces 
            then Mlist.of_list (List.map trm_seq (List.rev partition))
            else Mlist.of_list (List.map (fun x -> trm_seq_no_brace (Mlist.to_list x)) (List.rev partition))
            in
        
        if not braces then trm_seq_no_brace ~marks:t.marks (Mlist.to_list new_tl) else trm_seq ~annot:t.annot ~marks:t.marks new_tl
        
  | _ -> fail t.loc "Sequence_core.partial_aux: expected a sequence to partition"

(* [partition blocks braces t p]: applies [partition_aux] at trm [t] with path [p]. *)
let partition (blocks : int list) (braces : bool): Target.Transfo.local =
  Target.apply_on_path (partition_aux blocks braces)

(* [shiffle_aux braces t]: transposes a a list of partitioned sequences,
      [braces] - denotes a flag on the visibility of the added sequences,
      [t] - the ast of the complex sequence of blocks. *)
let shuffle_aux (braces : bool) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    if Mlist.length tl < 1 then fail t.loc "Sequence_core.shuffle_aux:can't shuffle an empty mlist";
    let first_row = Mlist.nth tl 0 in
    begin match first_row.desc with 
    | Trm_seq tl1 ->
      let loop_bound = Mlist.length tl1 in
      if loop_bound < 2 then fail t.loc "Sequence_core.shuffle_aux: expected a row of length at least 2";
      let global_acc = ref [] in
      for i = 0 to loop_bound-1 do
        let local_acc = Mlist.fold_left (fun acc t1 -> 
            begin match t1.desc with 
            | Trm_seq tl2 ->
              if Mlist.length tl2 <> loop_bound then fail t1.loc "Sequence_core.shuffle_aux: all the subgroups 
                                                                  should be of the same size";
              let temp_el = Mlist.nth tl2 i in
              let temp_el = 
              if braces 
                then Internal.remove_nobrace_if_sequence temp_el 
                else Internal.set_nobrace_if_sequence temp_el in
            temp_el :: acc
            | _ -> fail t1.loc "Sequence_core.shuffle_aux: all the elements of the blocks should be sequences"
            end
            
          ) [] tl in
        let local_acc = List.rev local_acc in
        global_acc := (if braces then trm_seq (Mlist.of_list local_acc) else trm_seq_no_brace local_acc) :: !global_acc
      done;
       trm_seq ~annot:t.annot ~marks:t.marks (Mlist.of_list (List.rev !global_acc))

    | _ -> fail first_row.loc "Sequence_core.shuffle_aux: shuffle can be applied only on sequences"
    end
  | _ -> fail t.loc "Sequence_core.shuffle_aux: expected the sequence with blocks to reorder"

(* [shuffle braces t p]: applies [shuffle_aux] at trm [t] with path [p]. *)
let shuffle (braces : bool) : Target.Transfo.local = 
  Target.apply_on_path (shuffle_aux braces) 
