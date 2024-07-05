open Prelude

let is_pure_ghost_call (t: trm): bool =
  trm_has_cstyle GhostCall t && Var_map.for_all (fun _ usage -> usage = Required || usage = Ensured) (Resources.usage_of_trm t)

(** Moves all pure ghosts upwards, starting from upmost ones. *)
let move_all_upwards_in (seq : trm) : trm =
  let error = "Ghost.move_all_pure_upwards: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let pures = ref [] in
  Mlist.iteri (fun i instr ->
    if is_pure_ghost_call instr then
      pures := i :: !pures;
    ) instrs;
  let pures = List.rev !pures in
  List.fold_left (fun seq pure_i ->
    Ghost.move_up_in_seq pure_i seq
  ) seq pures

let%transfo move_all_upwards (tg: target): unit =
  (* LATER: Should probably be replaced by a transfo that also eliminates the redundant ghosts at the top of the sequence *)
  Resources.ensure_computed ();
  Target.apply_at_target_paths move_all_upwards_in tg;
  Resources.justif_correct "only changed ghost code"

(** Moves all pure ghost downwards, starting from downmost ones.
    If they reach the bottom of the sequence and are not used in a post-condition
    instantiation, remove them *)
let minimize_all_in (seq : trm) : trm =
  let error = "expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let pures_rev = ref [] in
  Mlist.iteri (fun i instr ->
    if is_pure_ghost_call instr && not (Resource_trm.is_ghost_alias instr) then
      pures_rev := i :: !pures_rev;
    ) instrs;
  let post_inst_usage = Option.map Resource_computation.used_set_to_usage_map seq.ctx.ctx_resources_post_inst in
  List.fold_left (fun seq pure_i ->
    let desired_pos = Ghost.farthest_commuting_pos pure_i 1 seq in
    (* A pure fact at the bottom of a sequence can be removed
        if the post instantiation does not use it. *)
    (* We need to do this here because other pure ghosts
        may become the last instruction afterwards without
        beeing deleted themselves *)
    let delete_pure =
      match post_inst_usage with
      | None -> false (* LATER: Should we move out of the sequence in this case ? *)
      | Some post_inst_usage ->
        let instrs = trm_inv trm_seq_inv seq in
        if desired_pos <> Mlist.length instrs then
          false
        else
          let pure_usage = Resources.usage_of_trm (Mlist.nth instrs pure_i) in
          Var_map.is_empty (Resources.collect_interferences pure_usage post_inst_usage)
    in
    if delete_pure then
      Sequence_core.delete_at pure_i seq
    else if desired_pos <= pure_i + 1 then
      seq
    else
      Instr_core.move_at desired_pos pure_i seq
  ) seq !pures_rev

let%transfo minimize_all_in_seq (tg: target): unit =
  Resources.ensure_computed ();
  Target.apply_at_target_paths minimize_all_in tg;
  Resources.justif_correct "only changed ghost code"

let fission_at (mark_between: mark) (split_i: int) (seq: trm) : trm =
  let error = "Ghost_pair.fission_at: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let tl1, tl2 = Mlist.split split_i instrs in

  let pure_ghosts1 = List.filter_map (fun instr ->
    if is_pure_ghost_call instr then
      Some (trm_copy instr)
    else
      None) (Mlist.to_list tl1) in
  trm_like ~old:seq (trm_seq_helper [TrmMlist tl1; Mark mark_between; TrmList pure_ghosts1; TrmMlist tl2])

(** Duplicates the pure ghosts at the targeted sequence interstice, to remove dependencies between the two parts of the sequence. *)
let%transfo fission ?(mark_between : mark = no_mark) (tg : target) : unit =
  Resources.ensure_computed ();
  Target.iter (fun p_before ->
    let (p_seq, split_i) = Path.extract_last_dir_before p_before in
    apply_at_path (fission_at mark_between split_i) p_seq
  ) tg;
  Resources.justif_correct "only changed ghost code"

let copy_inside_from_seq (index: int) (seq: trm): trm =
  let error = "Ghost_pair.copy_inside_from_seq: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let tl_before, t, tl_after = Mlist.get_item_and_its_relatives index instrs in

  let pure_ghosts_before = List.filter_map (fun instr ->
    if is_pure_ghost_call instr then
      Some (trm_copy instr)
    else
      None) (Mlist.to_list tl_before) in

  let new_t = Pattern.pattern_match t [
    Pattern.(trm_for !__ (trm_seq !__) !__) (fun range body contract () ->
      trm_like ~old:t (trm_for ~contract range (trm_seq_helper [TrmList pure_ghosts_before; TrmMlist body]))
    );
    (* LATER: Manage other kinds of terms with a notion of inside *)
    Pattern.__ (fun () -> failwith "Ghost_pair.copy_inside_from_seq: the targetted item is not handled")
  ] in

  trm_like ~old:seq (trm_seq_helper [TrmMlist tl_before; Trm new_t; TrmMlist tl_after])

(** Copies all the pure ghosts of the surrounding sequence at the begining of the body of the targetted instruction. *)
let%transfo copy_surrounding_inside (tg: target): unit =
  Target.apply_at_target_paths_in_seq copy_inside_from_seq tg;
  Resources.justif_correct "only changed ghost code"
