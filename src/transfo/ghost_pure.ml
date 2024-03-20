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
  let error = "Ghost_pure.move_all_downwards: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let pures = ref [] in
  Mlist.iteri (fun i instr ->
    if is_pure_ghost_call instr then
      pures := i :: !pures;
    ) instrs;
  let post_inst = seq.ctx.ctx_resources_post_inst in
  List.fold_left (fun seq pure_i ->
    let desired_pos = Ghost.farthest_commuting_pos pure_i 1 seq in
    printf "%d -> %d\n" pure_i desired_pos;
    (* A pure fact at the bottom of a sequence can be removed
        if the post instantiation does not use it. *)
    (* We need to do this here because other pure ghosts
        may become the last instruction afterwards without
        beeing deleted themselves *)
    let delete_pure =
      match post_inst with
      | None -> false (* LATER: Should we move out of the sequence in this case ? *)
      | Some post_inst ->
        let instrs = trm_inv trm_seq_inv seq in
        if desired_pos <> Mlist.length instrs then
          false
        else
          let pure_usage = Resources.usage_of_trm (Mlist.nth instrs pure_i) in
          let post_inst_usage = Resource_computation.used_set_to_usage_map post_inst in
          Hyp_map.is_empty (Resources.collect_interferences pure_usage post_inst_usage)
    in
    if delete_pure then
      Sequence_core.delete_at pure_i seq
    else if desired_pos <= pure_i + 1 then
      seq
    else
      Instr_core.move_at desired_pos pure_i seq
  ) seq !pures

let%transfo minimize_all_in_seq (tg: target): unit =
  Resources.ensure_computed ();
  Target.apply_at_target_paths minimize_all_in tg;
  Resources.justif_correct "only changed ghost code"
