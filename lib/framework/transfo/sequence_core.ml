open Prelude
open Target

(** [insert_at index code t]: inserts trm [code] at index [index] in sequence [t],
    [index] - a valid index where the instruction can be added,
    [code] - instruction to be added as an arbitrary trm,
    [t] - ast of the outer sequence where the insertion will be performed. *)
let insert_at (code : trm) (index : int) (t : trm) : trm =
  let error = "Sequence_core.insert_aux: expected the sequence on where insertion is performed." in
  let tl, result = trm_inv ~error trm_seq_inv t in
  let new_tl = Mlist.insert_at index code tl in
  (* TODO: Should use alter here ? *)
  trm_seq ~annot:t.annot ?result new_tl

(** [delete_at span t_seq]: deletes a span of instructions inside a sequence. *)
let delete_at (span : Dir.span) (t_seq : trm) : trm =
  update_span_helper span t_seq (fun _ -> [])

(** [intro_at index nb t]: regroups instructions with indices falling in the range \[index, index + nb) into a sub-sequence,
       [mark] - mark to insert on the new sub-sequence,
       [label] - a label to insert on the new sub-sequence,
       [index] - index where the grouping is performed,
       [nb] - number of instructions to consider,
       [t] - ast of the outer sequence where the insertion is performed.

  Correct if the variables bound in the sub-sequence are only used locally, i.e. there is no scope interference with the outer sequence continuation.
*)
let intro_at (mark : string) (label : label) (index : int) (nb : int) (t : trm) : trm =
  let error = "Sequence_core.intro_on: expected the sequence on which the grouping is performed." in
  let tl, result = trm_inv ~error trm_seq_inv t in
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
  trm_seq ~annot:t.annot ?result (Mlist.insert_at index intro_seq tl_around)

(** [elim_on t]: inlines an inner sequence into the outer one,
      [t] - ast of the sequence to be removed.

  This function does not perform the inlining, but simply tags the inner sequence as 'nobrace'.

  Correct if there is no name conflict between the variables bound in the  inner sequence and the ones used in the outer sequence continuation, i.e. there is no scope interference with the outer sequence continuation. This is checked by the code responsible for removing 'nobrace' sequences.
  *)
let elim_on (t : trm) : trm =
  let error = "Sequence_core.elim_on: expected a sequence to be deleted." in
  let tl, seqres = trm_inv ~error trm_seq_inv t in
  if Option.is_some seqres then failwith "Sequence_core.elim_on: cannot delete a sequence that has a result value, use elim_inside instead.";
  trm_pass_labels t (trm_seq_nobrace tl)

(* [change_binding x f_bind instrs]: change the binding of [x] that should appear a the toplevel of the list of instructions [instrs] according to the result of [f_bind] *)
let change_binding (x: var) (f_bind: typ -> trm -> trm) (instrs: trm Mlist.t): trm Mlist.t =
  let found_binding = ref false in
  let instrs' = Mlist.map (fun t -> match trm_let_inv t with
      | Some (v, ty, expr) when var_eq v x ->
        if !found_binding then failwith "Sequence_core.change_binding: found the binding of %s twice" (var_to_string x);
        found_binding := true;
        f_bind ty expr
      | _ -> t
    ) instrs in
  if not (!found_binding) then failwith "Sequence_core.change_binding: could not find a binding for %s" (var_to_string x);
  instrs'

(** [elim_inside_let local_path t]: inlines a term [t] of the form [let x = { t1; ...; tn; v };] in the surrounding sequence
      [t] - ast of term containing the let binding of the sequence
  This function does not really perform the inlining, but return a 'nobrace' sequence.
*)
let elim_inside_let ?(mark_result: mark = no_mark) (t: trm) : trm =

  if !Flags.debug_ocaml then begin
    Printf.printf "Calling elim_inside_let on : \n";
    let s = {Ast_to_text.default_style with print_var_id = true} in
    let ast_style = s in
    print_string (Ast_to_text.ast_to_string ~style:ast_style t);
    Printf.printf "\n end. \n";
  end;

  Pattern.pattern_match t [
    Pattern.(trm_let !__ !__ (trm_seq !__ (some !__))) (fun x ty instrs seqres () ->
      trm_pass_labels t (trm_seq_nobrace (change_binding seqres (fun tyres resexpr ->
        let ty = if is_typ_auto ty then tyres else ty in
        trm_add_mark mark_result (trm_let (x, ty) resexpr)) instrs))
    );
    Pattern.__ (fun () -> trm_fail t "Sequence_core.elim_inside_let: expected a sequence with result inside a let binding")
  ]

(** [wrap_on visible mark t]: surround [t] with a sequence,
    [mark] - mark to be added on the introduced sequence,
    [visible] - a flag on the visibility of the introduced sequence,
    [t] - any trm. *)
(* TODO: Generalize with spans *)
let wrap_on (mark : mark) (label : label) (visible : bool) (t : trm) : trm =
  (* FIXME: Bad behaviour if t is not of type void, we may want to generate { res = t; t } or to fail *)
  let wrapped_seq = if visible then trm_seq_nomarks [t] else trm_seq_nobrace_nomarks [t] in
  let marked_seq = trm_add_mark mark wrapped_seq in
  trm_add_label label marked_seq


(** [unwrap_on t]: the opposite of [wrap]
     [t] - a term that corresponds to a sequence with a single item in t. *)
(* TODO: Is this useful ? shouldn't [elim_on] always be used ? *)
let unwrap_on (t : trm) : trm =
  let error = "Sequence_core.unwrap: expected to operate on a sequence." in
  let tl, result = trm_inv ~error trm_seq_inv t in
  if Option.is_some result then failwith "Sequence_core.unwrap_on: cannot unwrap a sequence with return value";
  if Mlist.length tl = 1 then Mlist.nth tl 0
    else trm_fail t "Sequence_core.unwrap: can only unwrap a sequence with exactly one item"


(** [split_at index t]: splits [t] into two sequences,
      [index] - the location where the splitting is done,
      [is_fun_body] - flag used when splitting function bodies,
      [t] - trm that corresponds to the the targeted sequence. *)
let split_at (index : int) (is_fun_body : bool) (t : trm) : trm =
  let error = "Sequence_core.split_at: expected a sequence, containing the location where it is going to be splitted." in
  let tl, result = trm_inv ~error trm_seq_inv t in
  if Option.is_some result then failwith "Sequence_core.split_at: cannot split a sequence with return value";
  let first_part, last_part = Mlist.split index tl in
  let res =
    trm_seq_nobrace_nomarks [trm_seq first_part; trm_seq last_part] in
  if is_fun_body then trm_seq ~annot:t.annot (Mlist.of_list [res]) else res


(** [partition_on blocks braces]: partitions sequence [t] into a list of sequences,
      [blocks] -  a list of integers, where each integer denotes the size of the partition blocks,
      [braces] - flag on the visibility of the generated sequences,
      [t]: trm corresponding to a sequence. *)
let partition_on (blocks : int list) (braces : bool) (t : trm) : trm =
  let error = "Sequence_core.partition: expected a sequence to partition." in
  let tl, result = trm_inv ~error trm_seq_inv t in
  if Option.is_some result then failwith "Sequence_core.partition: cannot partition a sequence with result value";
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
      let seq = if braces then trm_seq new_tl else trm_seq_nobrace new_tl in
      trm_pass_marks t seq


(** [shuffle_on braces t]: transposes a a list of partitioned sequences,
      [braces] - denotes a flag on the visibility of the added sequences,
      [t] - the ast of the complex sequence of blocks. *)
let shuffle_on (braces : bool) (t : trm) : trm =
  let error = "Sequence_core.shuffle_aux: expected the sequence with blocks to reorder." in
  let tl, result = trm_inv ~error trm_seq_inv t in
  if Option.is_some result then failwith "Sequence_core.shuffle_aux: expected a sequence without result value";
  if Mlist.length tl < 1 then trm_fail t "Sequence_core.shuffle_aux: can't shuffle an empty mlist";
  let first_row = Mlist.nth tl 0 in
  begin match first_row.desc with
  | Trm_seq (tl1, None) ->
    let loop_bound = Mlist.length tl1 in
    if loop_bound < 2 then trm_fail t "Sequence_core.shuffle_aux: expected a row of length at least 2";
    let global_acc = ref [] in
    for i = 0 to loop_bound-1 do
      let local_acc = Mlist.fold_left (fun acc t1 ->
          begin match t1.desc with
          | Trm_seq (tl2, None) ->
            if Mlist.length tl2 <> loop_bound then trm_fail t1 "Sequence_core.shuffle_aux: all the subgroups
                                                                should be of the same size";
            let temp_el = Mlist.nth tl2 i in
            let temp_el = Nobrace.set_mark (not braces) temp_el in
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
