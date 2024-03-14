open Prelude
open Target

(* [insert_at index code t]: inserts trm [code] at index [index] in sequence [t],
    [index] - a valid index where the instruction can be added,
    [code] - instruction to be added as an arbitrary trm,
    [t] - ast of the outer sequence where the insertion will be performed. *)
let insert_at (code : trm) (index : int) (t : trm) : trm =
  let error = "Sequence_core.insert_aux: expected the sequence on where insertion is performed." in
  let tl = trm_inv ~error trm_seq_inv t in
  let new_tl = Mlist.insert_at index code tl in
  (* TODO: Should use alter here ? *)
  trm_seq ~annot:t.annot new_tl

(* [delete_at index nb_instr t]: deletes a number of instructions inside the sequence starting
      from [index] and ending at ([index] + [nb]),
      [index] - starting index,
      [nb] - number of instructions to delete,
      [t] - ast of the outer sequence where the deletion is performed. *)
let delete_at (index : int) ?(nb_instr : int = 1) (t : trm) : trm =
  let error = "Sequence_core.delete_aux: expected the sequence on which the trms are deleted." in
  let tl = trm_inv ~error trm_seq_inv t in
  trm_seq ~annot:t.annot (Mlist.remove index nb_instr tl)

(* [intro_aux index nb t]: regroups instructions with indices falling in the range [index, index + nb) into a sub-sequence,
       [mark] - mark to insert on the new sub-sequence,
       [label] - a label to insert on the new sub-sequence,
       [index] - index where the grouping is performed,
       [nb] - number of instructions to consider,
       [t] - ast of the outer sequence where the insertion is performed.

  Correct if the variables bound in the sub-sequence are only used locally, i.e. there is no scope interference with the outer sequence continuation.
*)
let intro_aux (mark : string) (label : label) (index : int) (nb : int) (t : trm) : trm =
  let error = "Sequence_core.intro_aux: expected the sequence on which the grouping is performed." in
  let tl = trm_inv ~error trm_seq_inv t in
  let index, nb = if nb < 0 then (index + nb + 1, -nb) else (index, nb) in
  let tl_before, tl_rest = Mlist.split index tl in
  let tl_seq, tl_after = Mlist.split nb tl_rest in
  if !Flags.check_validity then begin
    Scope.assert_no_interference ~after_what:"the new sequence" ~on_interference:"out of scope" tl_seq tl_after;
    Trace.justif "local variables are not used after the new sequence"
  end;
  let tl_around = Mlist.merge tl_before tl_after in
  let intro_seq = trm_seq tl_seq in
  let intro_seq = trm_add_mark mark intro_seq in
  let intro_seq = trm_add_label label intro_seq in
  trm_seq ~annot:t.annot (Mlist.insert_at index intro_seq tl_around)

(* [intro mark label index nb t p]: applies [intro_aux] at trm [t] with path [p]. *)
let intro (mark : string) (label : label) (index : int) (nb : int) : Transfo.local =
  apply_on_path (intro_aux mark label index nb)

(* [elim_aux index t]: inlines an inner sequence into the outer one,
      [t] - ast of the sequence to be removed.

  This function does not perform the inlining, but simply tags the inner sequence as 'nobrace'.

  Correct if there is no name conflict between the variables bound in the  inner sequence and the ones used in the outer sequence continuation, i.e. there is no scope interference with the outer sequence continuation. This is checked by the code responsible for removing 'nobrace' sequences.
  *)
let elim_aux (t : trm) : trm =
  let error = "Sequence_core.elim_aux: expected the sequence to be deleteds." in
  let tl = trm_inv ~error trm_seq_inv t in
  trm_pass_labels t (trm_seq_nobrace tl)

(* [elim t p]: applies [elim_aux] at trm [t] with path [p]. *)
let elim : Transfo.local =
  apply_on_path elim_aux

(* [intro_on_instr_aux visible mark t]: surround [t] with a sequence,
    [mark] - mark to be added on the introduced sequence,
    [visible] - a flag on the visibility of the introduced sequence,
    [t] - any trm. *)
let intro_on_instr_aux (mark : mark) (label : label) (visible : bool) (t : trm) : trm =
  let wrapped_seq = if visible then trm_seq_nomarks [t] else trm_seq_nobrace_nomarks [t] in
  let marked_seq = trm_add_mark mark wrapped_seq in
  trm_add_label label marked_seq

(* [intro_on_instr visible mark t p]: applies [intro_on_instr_aux] at trm [t] with path [p]. *)
let intro_on_instr (visible : bool) (mark : mark) (label : label) : Transfo.local=
  apply_on_path (intro_on_instr_aux mark label visible)

(* [unwrap_aux t]: the opposite of [intro_on_instr_aux]
     [t] - a term that corresponds to a sequence with a single item in t. *)
let unwrap_aux (t : trm) : trm =
  let error = "Sequence_core.unwrap_aux: expected to operate on a sequence." in
  let tl = trm_inv ~error trm_seq_inv t in
  if Mlist.length tl = 1 then Mlist.nth tl 0
    else trm_fail t "Sequence_core.unwrap_aux: can only unwrap a sequence with exactly one item"

(* [unwrap t p]: applies [unwrap_aux] at trm [t] with path [p]. *)
let unwrap : Transfo.local =
  apply_on_path unwrap_aux

(* [split_aux index t ]: splitts [t] into two sequences,
      [index] - the location where the splitting is done,
      [is_fun_body] - flag used when splitting function bodies,
      [t] - trm that corresponds to the the targeted sequence. *)
let split_aux (index : int) (is_fun_body : bool) (t : trm) : trm =
  let error = "Sequence_core.split_aux: expected a sequence, containing the location where it is going to be splitted." in
  let tl = trm_inv ~error trm_seq_inv t in
  let first_part,last_part = Mlist.split index tl in
  let res =
  trm_seq_nobrace_nomarks [trm_seq first_part; trm_seq last_part] in
  if is_fun_body then trm_seq ~annot:t.annot (Mlist.of_list [res]) else res

(* [split index is_fun_body t p]: applies [split_aux] at trm [t] with path [p]. *)
let split (index : int) (is_fun_body : bool) : Transfo.local =
  apply_on_path (split_aux index is_fun_body)

(* [partition blocks braces]: partitions sequence [t] into a list of sequences,
      [blocks] -  a list of integers, where each integer denotes the size of the partition blocks,
      [braces] - flag on the visibility of the generated sequences,
      [t]: trm corresponding to a sequence. *)
let partition_aux (blocks : int list) (braces : bool) (t : trm) : trm =
  let error = "Sequence_core.partial_aux: expected a sequence to partition." in
  let tl = trm_inv ~error trm_seq_inv t in
  let nb = Mlist.length tl in
  let blocks = if blocks = [] then [nb] else blocks in
  let sum_blocks = List.fold_left (+) 0 blocks in
  if sum_blocks <> nb
    then trm_fail t (Printf.sprintf "Sequence_core.partition: the partition entered is not correct,
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
          else Mlist.of_list (List.map trm_seq_nobrace (List.rev partition))
        in
      let seq = if braces then trm_seq new_tl else trm_seq_nobrace tl in
      trm_pass_marks t seq

(* [partition blocks braces t p]: applies [partition_aux] at trm [t] with path [p]. *)
let partition (blocks : int list) (braces : bool): Transfo.local =
  apply_on_path (partition_aux blocks braces)

(* [shiffle_aux braces t]: transposes a a list of partitioned sequences,
      [braces] - denotes a flag on the visibility of the added sequences,
      [t] - the ast of the complex sequence of blocks. *)
let shuffle_aux (braces : bool) (t : trm) : trm =
  let error = "Sequence_core.shuffle_aux: expected the sequence with blocks to reorder." in
  let tl = trm_inv ~error trm_seq_inv t in
  if Mlist.length tl < 1 then trm_fail t "Sequence_core.shuffle_aux:can't shuffle an empty mlist";
  let first_row = Mlist.nth tl 0 in
  begin match first_row.desc with
  | Trm_seq tl1 ->
    let loop_bound = Mlist.length tl1 in
    if loop_bound < 2 then trm_fail t "Sequence_core.shuffle_aux: expected a row of length at least 2";
    let global_acc = ref [] in
    for i = 0 to loop_bound-1 do
      let local_acc = Mlist.fold_left (fun acc t1 ->
          begin match t1.desc with
          | Trm_seq tl2 ->
            if Mlist.length tl2 <> loop_bound then trm_fail t1 "Sequence_core.shuffle_aux: all the subgroups
                                                                should be of the same size";
            let temp_el = Mlist.nth tl2 i in
            let temp_el =
            if braces
              then Nobrace.remove_if_sequence temp_el
              else Nobrace.set_if_sequence temp_el in
          temp_el :: acc
          | _ -> trm_fail t1 "Sequence_core.shuffle_aux: all the elements of the blocks should be sequences"
          end

          ) [] tl in
        let local_acc = List.rev local_acc in
        global_acc := (if braces then trm_seq (Mlist.of_list local_acc) else trm_seq_nobrace_nomarks local_acc) :: !global_acc
      done;
       trm_seq ~annot:t.annot (Mlist.of_list (List.rev !global_acc))
  | _ -> trm_fail first_row "Sequence_core.shuffle_aux: shuffle can be applied only on sequences"
  end

(* [shuffle braces t p]: applies [shuffle_aux] at trm [t] with path [p]. *)
let shuffle (braces : bool) : Transfo.local =
  apply_on_path (shuffle_aux braces)
