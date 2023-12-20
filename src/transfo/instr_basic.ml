open Prelude
open Target

(* [delete tg]: expects the target [tg] to point at an instruction inside a sequence
      then it will remove that instruciton from that sequence *)
let%transfo delete (tg : target) : unit =
  Sequence_basic.delete tg

(* [copy ~target tg]: expects the target [tg] to point at an instruction that is
    going to be copied to the relative target [where]. If [delete] is true then
    the targetd instruction will be delete. *)
let%transfo copy ?(mark_copy : mark = no_mark) ?(rev : bool = false) ?(delete : bool = false) ?(dest:target = []) (tg : target) : unit =
  Target.apply_on_transformed_targets ~rev (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) ->
      let tg_dest_path_seq, dest_index = if dest = [] then p, i+1 else Target.resolve_target_between_exactly_one dest t in
      if tg_dest_path_seq <> p then path_fail p "Instr_basic.copy: the destination target should be unique and belong to the same block as the main targets";
      Instr_core.copy mark_copy dest_index i delete t p) tg


(* [move ~rev ~dest tg]: expects the target [tg] to point at the instruction that is
    going to be moved at the relative target [where]. In case the target [tg] points to
    more than one instructions, to keep the original order of the instructions [rev] should be set to true

   @correctness: Correct if the swapped instructions are parallelizable:
   {H1 * H} instr1 {H1' * H} and {H2 * H} instr2 {H2' * H}
   which lead globally to the derivation
   {H1 * H2 * H} instr1 {H1' * H2 * H} instr2 {H1' * H2' * H}
   we can build the same postcondition with
   {H1 * H2 * H} instr2 {H1 * H2' * H} instr1 {H1' * H2' * H}

   This is sufficient but not necessary, a manual commutation proof can be used
   as well. *)
let%transfo move ?(mark : mark = no_mark) ?(rev : bool = false) ~dest:(dest : target) (tg : target) : unit =
  Resources.required_for_check ();
  Target.apply ~rev (fun t instr_p ->
    let (p_seq, i) = Internal.isolate_last_dir_in_seq instr_p in
    let dest_p_seq, dest_index = if dest = [] then p_seq, i+1 else Target.resolve_target_between_exactly_one dest t in
    if dest_p_seq <> p_seq then path_fail p_seq "Instr_basic.move: the destination target should be unique and belong to the same block as the main targets";
    if !Flags.check_validity then begin
      let instr_t = Path.resolve_path instr_p t in
      let (first_swapped_i, last_swapped_i, assert_seq_instrs_commute) =
        if i < dest_index
        then (i+1, dest_index-1, Resources.assert_seq_instrs_commute instr_t)
        else (dest_index, i-1, fun x -> Resources.assert_seq_instrs_commute x instr_t)
      in
      for swapped_instr_i = first_swapped_i to last_swapped_i do
        let swapped_instr_t = Path.resolve_path (p_seq @ [Dir_seq_nth swapped_instr_i]) t in
        assert_seq_instrs_commute swapped_instr_t
      done;
      Trace.justif "resources commute"
    end;
    Instr_core.copy mark dest_index i true t p_seq
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
