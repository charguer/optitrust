open Prelude
open Target
open Resource_formula
open Resource_contract
open Resources

(** <private> *)
let move_in_seq (i : int) (direction : int) (seq : trm) : trm =
  let error = "Ghost_pair.move_in_seq: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let instr = Mlist.nth instrs i in
  let current_i = ref i in
  let dest_offset, interference_with_instr =
    match direction with
    | -1 -> 0, fun next -> collect_interferences next instr
    | 1 -> 1, fun next -> collect_interferences instr next
    | _ -> failwith "Ghost_pair.move_in_seq: expected -1 or 1 direction"
  in
  let commutes_with_next () : bool =
    let next_i = !current_i + direction in
    let commutes = match Mlist.nth_opt instrs next_i with
    | Some next ->
      let interference = interference_with_instr next in
      let commutes = Hyp_map.is_empty interference in
      (* DEBUG:
      if not commutes then
        print_string (string_of_interference interference); *)
      commutes
    | None ->
      false
    in
    if commutes then current_i := next_i;
    commutes
  in
  while commutes_with_next () do () done;
  if i != !current_i then
    let dest_i = !current_i + dest_offset in
    Instr_core.copy_aux dest_i i true seq
  else
    seq

(** Moves instruction at index [i] in sequence [seq] as far down as possible, as long as effects commute.
    TODO: what about var ids and pure facts scopes?
   *)
let move_down_in_seq (i : int) (seq : trm) : trm = move_in_seq i 1 seq

(** Moves instruction at index [i] in sequence [seq] as far up as possible, as long as effects commute.
    TODO: what about var ids and pure facts scopes?
   *)
let move_up_in_seq (i : int) (seq : trm) : trm = move_in_seq i (-1) seq

(** <private>
    Moves all begins downwards, starting from downmost ones. *)
let move_all_begins_downwards (seq : trm) : trm =
  let error = "Ghost_pair.move_all_begins_downwards: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let begins = ref [] in
  let find_begins i instr =
    match trm_ghost_begin_inv instr with
    | Some _ -> begins := i :: !begins;
    | None -> ()
  in
  Mlist.iteri find_begins instrs;
  let upwards_begins = !begins in
  (* Printf.printf "upwards_begins: %s\n" (Tools.list_to_string (List.map string_of_int upwards_begins)); *)
  List.fold_left (fun seq beg_i ->
    move_down_in_seq beg_i seq
  ) seq upwards_begins

(** <private>
    Moves all ends upwards, starting from upwardmost ones. *)
let move_all_ends_upwards (seq : trm) : trm =
  let error = "Ghost_pair.move_all_ends_upwards: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let ends = ref [] in
  let find_ends i instr =
    match trm_ghost_end_inv instr with
    | Some _ -> ends := i :: !ends;
    | None -> ()
  in
  Mlist.iteri find_ends instrs;
  let downwards_ends = List.rev !ends in
  (* Printf.printf "downwards_ends: %s\n" (Tools.list_to_string (List.map string_of_int downwards_ends)); *)
  List.fold_left (fun seq end_i ->
    move_up_in_seq end_i seq
  ) seq downwards_ends

(** <private>
    Cancels all ghost pairs that have an empty scope, starting from innermost ones. *)
let cancel_all_ghost_pairs (seq : trm) : trm =
  let error = "Ghost_pair.cancel_all_ghost_pairs: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let begins_stack = ref [] in (* stack of open begins vars and indices *)
  let to_delete = ref [] in (* upwards list of indices *)
  let populate_to_delete i instr =
    let i_with_delete = i - List.length !to_delete in
    match trm_ghost_begin_inv instr with
    | Some (gv, _, _) ->
      begins_stack := (gv, i, i_with_delete) :: !begins_stack;
    | None ->
      begin match trm_ghost_end_inv instr with
      | Some gv ->
        begin match !begins_stack with
        | (gv_beg, i_beg, i_beg_with_delete) :: bs_rest when gv = gv_beg ->
          begins_stack := bs_rest;
          if i_beg_with_delete + 1 == i_with_delete then
            to_delete := i :: i_beg :: !to_delete
        | _ ->
          (* unbalanced ghost pairs, or no matching ghost begin *)
          ()
        end
      | None -> ()
      end
  in
  Mlist.iteri populate_to_delete instrs;
  to_delete := List.rev !to_delete; (* downwards list of indices *)
  let instrs' = Mlist.filteri (fun i _ ->
    match !to_delete with
    | tdi :: tdr when i = tdi ->
      to_delete := tdr;
      false
    | _ ->
      true
  ) instrs in
  trm_seq ~annot:seq.annot ?loc:seq.loc instrs'

(** Minimizes the scope of ghost pairs in the given sequence. *)
let minimize_all_on_seq (seq : trm) : trm =
  let seq = move_all_begins_downwards seq in
  let seq = move_all_ends_upwards seq in
  let seq = cancel_all_ghost_pairs seq in
  seq

(** Minimizes the scope of ghost pairs in the targeted sequence. *)
let%transfo minimize_all_in_seq (tg : target) : unit =
  recompute_all_resources ();
  Target.apply_at_target_paths minimize_all_on_seq tg;
  Trace.apply Scope.infer_var_ids (* FIXME: move up/down should avoid breaking scopes *)

(** <private>
    cf. [fission]. *)
let fission_on (split_i : int) (seq : trm) : trm =
  let error = "Ghost_pair.fission_on: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let tl1, tl2 = Mlist.split split_i instrs in

  (* 1. Find all scope begins before split point. *)
  let begins_stack = ref [] in
  let find_begins instr =
    match trm_ghost_begin_inv instr with
    | Some gbi -> begins_stack := gbi :: !begins_stack;
    | None ->
      begin match trm_ghost_end_inv instr with
      | Some gv ->
        begin match !begins_stack with
        | (gv_beg, _, _) :: bs_rest when gv = gv_beg ->
          begins_stack := bs_rest
        | _ ->
          (* unbalanced ghost pairs, or no matching ghost begin *)
          (* TODO: what should happen in this case? *)
          ()
        end
      | None -> ()
      end
  in
  Mlist.iter find_begins tl1;

  (* 2. End all opened scopes before split point. *)
  let end_begin (gv, _, _) = trm_ghost_end gv in
  let ends = Mlist.of_list (List.map end_begin !begins_stack) in

  (* 3. Re-open scopes after split point. *)
  let subst_after_split = ref Var_map.empty in
  let re_begin (gv, v, args) =
    let gv' = generate_ghost_pair_var () in
    let g_beg = trm_ghost_begin gv' v args in
    subst_after_split := Var_map.add gv (trm_var gv') !subst_after_split;
    g_beg
  in
  let begins = Mlist.of_list (List.rev_map re_begin !begins_stack) in
  let tl2' = Mlist.map (trm_subst !subst_after_split) tl2 in

  (* 4. Construct resulting sequence. *)
  let instrs' = Mlist.merge_list [tl1; ends; begins; tl2'] in
  trm_seq ~annot:seq.annot ?loc:seq.loc instrs'

(** Distributes the scope of ghost pairs at the targeted sequence interstice. *)
let%transfo fission (tg : target) : unit =
  Target.apply (fun t p_before ->
    let (p_seq, split_i) = Path.last_dir_before_inv_success p_before in
    apply_on_path (fission_on split_i) t p_seq
  ) tg;
  justif_correct "ghosts where successfully distributed"
