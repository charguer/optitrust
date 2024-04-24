open Prelude
open Target

(* [delete tg]: expects the target [tg] to point at an instruction inside a sequence
      then it will remove that instruciton from that sequence *)
let delete = Sequence_basic.delete

(* [copy ~target tg]: expects the target [tg] to point at an instruction that is
    going to be copied to the target [dest].
    TODO: Either support copying at arbitrary location or use a relative target *)
let%transfo copy ?(mark_copy : mark = no_mark) ?(rev : bool = false) ?(dest:target = []) (tg : target) : unit =
  Target.iter ~rev (fun path ->
      let (i,p_seq) = Path.index_in_seq path in
      let tg_dest_path_seq, dest_index = if dest = [] then p_seq, i+1 else Target.resolve_target_between_exactly_one dest in
      if tg_dest_path_seq <> p_seq then path_fail p_seq "Instr_basic.copy: the destination target should be unique and belong to the same block as the main targets";
      Target.apply_at_path (Instr_core.copy_at mark_copy dest_index i) p_seq) tg


(* [move ~rev ~dest tg] move the instructions at target [tg] to the relative position [dest].
    If you want to move contiguous instructions together, you can use a span target.

    The relative position [dest] is computed independently for each target so it is
    allowed to have matches inside the same sequence, inside different sequences,
    or a mix between the two as long as you can write a common [dest] target.

    If [rev] is true, perform the moves in the reverse order if the target resolves
    to multiple paths.

    TODO: decide and specify how marks between instructions are moved in the sequence

   @correctness: Correct if the swapped instructions are parallelizable:
   {H1 * H} instr1 {H1' * H} and {H2 * H} instr2 {H2' * H}
   which lead globally to the derivation
   {H1 * H2 * H} instr1 {H1' * H2 * H} instr2 {H1' * H2' * H}
   we can build the same postcondition with
   {H1 * H2 * H} instr2 {H1 * H2' * H} instr1 {H1' * H2' * H}

   This is sufficient but not necessary, a manual commutation proof can be used
   as well. *)
let%transfo move ?(rev : bool = false) ~(dest : target) (tg : target) : unit =
  Resources.required_for_check ();
  Target.iter ~rev (fun p ->
    let p_seq, span = Path.extract_last_dir_span p in
    Target.apply_at_path (fun t_seq ->
      let dest_index = match Constr.resolve_target_between_children dest t_seq with
        | [i] -> i
        | [] -> trm_fail t_seq "Instr_basic.move: could not find the destination target";
        | _ -> trm_fail t_seq "Instr_basic.move: the destination target should be unique";
      in
      let before_index, mid_index, after_index =
        if span.start < dest_index then begin
          if span.stop > dest_index then trm_fail t_seq "Instr.basic.move: the destination should be outside the moved span";
          (span.start, span.stop, dest_index)
        end else
          (dest_index, span.start, span.stop)
      in

      let seq = trm_inv trm_seq_inv t_seq in
      let seq, untouched_after = Mlist.split ~left_bias:false after_index seq in
      let seq, swapped_after = Mlist.split mid_index seq in
      let untouched_before, swapped_before = Mlist.split ~left_bias:true before_index seq in

      if !Flags.check_validity then begin
        let usage_before = Resources.compute_usage_of_instrs swapped_before in
        let usage_after = Resources.compute_usage_of_instrs swapped_after in
        Resources.assert_usages_commute t_seq.loc usage_before usage_after;
        Trace.justif "resources commute"
      end;

      let seq = Mlist.merge_list [untouched_before; swapped_after; swapped_before; untouched_after] in
      trm_alter ~desc:(Trm_seq seq) t_seq
    ) p_seq
  ) tg;
  Scope.infer_var_ids ()

(* [read_last_write ~write tg]: expects the target [tg] to point at a read operation,
    then it replaces the read operation with left hand side of the write operation targeted
    by [write]

   @correctness: the read expression must be pure, and its evaluation must not
   have changed since the write.*)
let%transfo read_last_write ~write:(write : target) (tg : target) : unit =
  let write_trm = match Target.get_trm_at write with
  | Some wt -> wt
  | None -> failwith "uninline: write target does point to any node" in
  let written_trm =
    match write_trm.desc with
    | Trm_apps (_, [_;rhs], _) when is_set_operation write_trm -> rhs
    | Trm_let (_, _, init) ->
      begin match get_init_val init with
      | Some init -> init
      | None -> trm_fail write_trm "Instr_basic.read_last_write: the targeted write operation should be either a set operation or
           or an initialized variable declaration"
      end
    | _ -> trm_fail write_trm "Instr_basic.read_last_write: the targeted write operation should be either a set operation or
      an initialized variable declaration" in
  Target.apply_on_targets (fun t p ->
    let get_op_path = Internal.get_ascendant_path (fun t -> (is_get_operation t)) p t in
    if get_op_path = [] then t else
    Target.apply_on_path (fun _ -> written_trm) t p) tg

(* [accumulate tg] expects the target [tg] to point at a block of write operations that write to the same memory location
    then accumulate those write operations into a single one.
    Ex.
    int x;
    {
      x += 1;
      x += 2;
      x += 3;
    }
    is transformed to x += 1+2+3 *)
let%transfo accumulate (tg : target) : unit =
  Target.apply_on_targets (Instr_core.accumulate) tg
