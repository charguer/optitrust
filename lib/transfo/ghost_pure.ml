open Prelude

let is_pure_ghost_call (t: trm): bool =
  trm_has_attribute GhostInstr t && Var_map.for_all (fun _ usage -> usage = Required || usage = Ensured) (Resources.usage_of_trm t)

let is_useless_pure_ghost_call (t: trm): bool =
  trm_has_attribute GhostInstr t && Var_map.for_all (fun _ usage -> usage = Required) (Resources.usage_of_trm t)

(** Moves all pure ghosts upwards, starting from upmost ones. *)
let move_all_upwards_in (seq : trm) : trm =
  let error = "Ghost_pure.move_all_upwards: expected sequence" in
  let instrs, _ = trm_inv ~error trm_seq_inv seq in
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

(** Moves all clears instructions upwards, stating from upmost ones.
    If a clears touches the instruction that ensured the fact,
    embed it in the ghost result binding list (as a new "_ <- hyp").
    If additionnally that instruction was a pure ghost and will not ensure
    any fact anymore, remove it. *)
let move_clears_upwards_in (seq: trm) : trm =
  let error = "Ghost_pure.move_clears_upwards: expected sequence" in
  let instrs, _ = trm_inv ~error trm_seq_inv seq in
  let clears = ref [] in
  Mlist.iteri (fun i instr ->
    match Resource_trm.ghost_clear_inv instr with
    | Some x -> clears := (i, x) :: !clears;
    | None -> ()
    ) instrs;
  let clears = List.rev !clears in
  let index_adjust = ref 0 in
  List.fold_left (fun seq (clear_i, cleared_var) ->
    let clear_i = clear_i + !index_adjust in
    let desired_pos = Ghost.farthest_commuting_pos clear_i (-1) seq in
    let instrs, result = trm_inv trm_seq_inv seq in
    try match Mlist.nth_opt instrs (desired_pos - 1) with
    | Some call ->
      let new_call = Ghost.rename_ghost_bind call cleared_var None in
      new_call.ctx.ctx_resources_usage <- Option.map (Var_map.remove cleared_var) new_call.ctx.ctx_resources_usage; (* For [is_useless_pure_ghost_call] detection *)
      decr index_adjust;
      let instrs = Mlist.remove clear_i 1 instrs in
      let instrs =
        if is_useless_pure_ghost_call new_call then begin
          decr index_adjust;
          Mlist.remove (desired_pos - 1) 1 instrs
        end else
          Mlist.replace_at (desired_pos - 1) new_call instrs
      in
      trm_seq instrs ?result
    | None -> raise Ghost.NotBound
    with Ghost.NotBound ->
      Instr_core.move_at desired_pos clear_i seq
  ) seq clears

let%transfo move_clears_upwards (tg: target): unit =
  Resources.ensure_computed ();
  Target.apply_at_target_paths move_clears_upwards_in tg;
  Resources.justif_correct "only changed ghost code"

(** Remove all clears with mark [mark] from the sequence [seq] *)
let remove_clears_in (mark: mark) (seq: trm): trm =
  let instrs, result = trm_inv ~error:"expected sequence" trm_seq_inv seq in
  let instrs = Mlist.filter (fun instr ->
    Option.is_none (Resource_trm.ghost_clear_inv instr) || not (trm_has_mark mark instr)
  ) instrs in
  trm_seq instrs ?result

let%transfo remove_clears (mark: mark) (tg: target): unit =
  Resources.ensure_computed ();
  Target.apply_at_target_paths (remove_clears_in mark) tg;
  Resources.justif_correct "only changed ghost code"

(** Move all clears upwards like in [move_clears_upwards_in] and then moves all pure ghost downwards, starting from downmost ones.
    If all components of a pure ghost are cleared, remove both pure ghost and clears.
    If they reach the bottom of the sequence and are not used in a post-condition
    instantiation, also remove them. *)
let minimize_all_in (seq : trm) : trm =
  let post_inst_usage = Option.map Resource_computation.used_set_to_usage_map seq.ctx.ctx_resources_post_inst in
  let seq = move_clears_upwards_in seq in
  let instrs, _ = trm_inv ~error:"expected sequence" trm_seq_inv seq in
  let pures_rev = ref [] in
  Mlist.iteri (fun i instr ->
    if is_pure_ghost_call instr && not (Resource_trm.is_ghost_alias instr) then
      pures_rev := i :: !pures_rev;
    ) instrs;
  List.fold_left (fun seq pure_i ->
    let desired_pos = Ghost.farthest_commuting_pos pure_i 1 seq in
    (* A pure fact at the bottom of a sequence can be removed
        if the post instantiation does not use it. *)
    (* We need to do this here because other pure ghosts
        may become the last instruction afterwards without
        beeing deleted themselves *)
    let delete_pure =
      match post_inst_usage with
      | None -> false (* LATER: Should we move the ghost out of the sequence when not instantiating a contract ? *)
      | Some post_inst_usage ->
        let instrs, _ = trm_inv trm_seq_inv seq in
        if desired_pos <> Mlist.length instrs then
          false
        else
          let pure_usage = Resources.usage_of_trm (Mlist.nth instrs pure_i) in
          Var_map.is_empty (Resources.collect_interferences pure_usage post_inst_usage)
    in
    if delete_pure then
      Sequence_core.delete_at (Dir.span_around pure_i) seq
    else if desired_pos <= pure_i + 1 then
      seq
    else
      Instr_core.move_at desired_pos pure_i seq
  ) seq !pures_rev

let%transfo minimize_all_in_seq (tg: target): unit =
  Resources.ensure_computed ();
  Target.apply_at_target_paths minimize_all_in tg;
  Resources.justif_correct "only changed ghost code"

let fission_at ~(mark_between: mark) ~(mark_clears: mark) (split_i: int) (seq: trm) : trm =
  let error = "Ghost_pair.fission_at: expected sequence" in
  let instrs, result = trm_inv ~error trm_seq_inv seq in
  let tl1, middle_marks, tl2 = Mlist.split_on_marks split_i instrs in

  let rev_new_pure_ghosts = ref [] in
  let vars_to_clear = ref [] in
  let subst_map = ref Var_map.empty in
  let already_cleared_set = ref Var_set.empty in

  let tl1 = Mlist.map (fun instr ->
      match Resource_trm.ghost_call_inv instr with
      | Some ghost when is_pure_ghost_call instr ->
        let effective_ghost_bind =
          match instr.ctx.ctx_resources_contract_invoc with
          | Some invoc -> List.filter (fun (bind_name, _) -> Option.is_none bind_name) ghost.ghost_bind @ List.map (fun (bind_name, contract_name) -> (Some bind_name, contract_name)) (Var_map.bindings invoc.contract_produced.contract_hyp_names)
          | None -> ghost.ghost_bind
        in

        let new_ghost_fn = trm_copy ghost.ghost_fn in
        let new_ghost_args = List.map (fun (x, t) -> (x, trm_copy (trm_vars_subst !subst_map t))) ghost.ghost_args in
        let new_ghost_bind = List.map (fun (x_out, x_contract) ->
            let new_out = Option.map (fun x_out ->
              let new_out = new_var x_out.name in
              subst_map := Var_map.add x_out new_out !subst_map;
              vars_to_clear := x_out :: !vars_to_clear;
              new_out) x_out
            in
            (new_out, x_contract)
          ) effective_ghost_bind
        in
        rev_new_pure_ghosts := Resource_trm.ghost { ghost_fn = new_ghost_fn; ghost_args = new_ghost_args; ghost_bind = new_ghost_bind } :: !rev_new_pure_ghosts;

        Resource_trm.ghost { ghost with ghost_bind = effective_ghost_bind }

      | _ ->
        begin match Resource_trm.ghost_clear_inv instr with
        | Some x ->
          begin match Var_map.find_opt x !subst_map with
          | Some new_x ->
            already_cleared_set := Var_set.add x !already_cleared_set;
            rev_new_pure_ghosts := Resource_trm.ghost_clear new_x :: !rev_new_pure_ghosts
          | None -> ()
          end
        | None -> ()
        end;
        instr
    ) tl1 in

  let new_pure_clears = List.filter_map (fun to_clear -> if Var_set.mem to_clear !already_cleared_set then None else Some (trm_add_mark mark_clears (Resource_trm.ghost_clear to_clear))) !vars_to_clear in
  let new_pure_ghosts = List.rev !rev_new_pure_ghosts in
  let tl2 = Mlist.map (trm_vars_subst !subst_map) tl2 in

  trm_like ~old:seq (trm_seq_helper ?result [TrmMlist tl1; TrmList new_pure_clears; Mark mark_between; MarkList middle_marks; TrmList new_pure_ghosts; TrmMlist tl2])

(** Duplicates the pure ghosts at the targeted sequence interstice, to remove dependencies between the two parts of the sequence. *)
let%transfo fission ?(mark_between: mark = no_mark) ?(mark_clears: mark = no_mark) (tg : target) : unit =
  Resources.ensure_computed ();
  Target.iter (fun p_before ->
    let (p_seq, split_i) = Path.extract_last_dir_before p_before in
    apply_at_path (fission_at ~mark_between ~mark_clears split_i) p_seq
  ) tg;
  Resources.justif_correct "only changed ghost code"

let copy_inside_from_seq (index: int) (seq: trm): trm =
  let error = "Ghost_pair.copy_inside_from_seq: expected sequence" in
  let instrs, result = trm_inv ~error trm_seq_inv seq in
  let tl_before, t, tl_after = Mlist.get_item_and_its_relatives index instrs in

  let pure_ghosts_before = List.filter_map (fun instr ->
    if is_pure_ghost_call instr then
      Some (trm_copy instr)
    else
      None) (Mlist.to_list tl_before) in

  let new_t = Pattern.pattern_match t [
    Pattern.(trm_for !__ (trm_seq !__ __) !__) (fun range body contract () ->
      trm_like ~old:t (trm_for ~contract range (trm_seq_helper [TrmList pure_ghosts_before; TrmMlist body]))
    );
    (* LATER: Manage other kinds of terms with a notion of inside *)
    Pattern.__ (fun () -> failwith "Ghost_pair.copy_inside_from_seq: the targetted item is not handled")
  ] in

  trm_like ~old:seq (trm_seq_helper ?result [TrmMlist tl_before; Trm new_t; TrmMlist tl_after])

(** Copies all the pure ghosts of the surrounding sequence at the begining of the body of the targetted instruction. *)
let%transfo copy_surrounding_inside (tg: target): unit =
  Target.apply_at_target_paths_in_seq copy_inside_from_seq tg;
  Resources.justif_correct "only changed ghost code"

let move_inside_from_seq (index: int) (seq: trm): trm =
  let error = "Ghost_pair.move_inside_from_seq: expected sequence" in
  let instrs, result = trm_inv ~error trm_seq_inv seq in
  let tl_before, t, tl_after = Mlist.get_item_and_its_relatives index instrs in

  let last_impure_instr = Mlist.fold_lefti (fun i last t ->
    if is_pure_ghost_call t then last else (i + 1)) 0 tl_before in
  let tl_before, pure_ghosts_before = Mlist.split last_impure_instr tl_before in

  let new_t = Pattern.pattern_match t [
    Pattern.(trm_for !__ (trm_seq !__ __) !__) (fun range body contract () ->
      trm_like ~old:t (trm_for ~contract range (trm_seq_helper [TrmMlist pure_ghosts_before; TrmMlist body]))
    );
    (* LATER: Manage other kinds of terms with a notion of inside *)
    Pattern.__ (fun () -> failwith "Ghost_pair.move_inside_from_seq: the targetted item is not handled")
  ] in

  trm_like ~old:seq (trm_seq_helper ?result [TrmMlist tl_before; Trm new_t; TrmMlist tl_after])

(** Move all the pure ghosts immediately surrounding the targetted instruction inside this instruction. *)
let%transfo move_surrounding_inside (tg: target): unit =
  Target.apply_at_target_paths_in_seq move_inside_from_seq tg;
  Resources.justif_correct "only changed ghost code"
