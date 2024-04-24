open Prelude
open Target
open Resource_formula
open Resource_contract
open Resources

(** <private>
    Moves all begins downwards, starting from downmost ones. *)
let move_all_begins_downwards (seq : trm) : trm =
  let error = "Ghost_pair.move_all_begins_downwards: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let begins = ref [] in
  let find_begins i instr =
    match Resource_trm.ghost_begin_inv instr with
    | Some _ -> begins := i :: !begins;
    | None -> ()
  in
  Mlist.iteri find_begins instrs;
  let upwards_begins = !begins in
  (* Printf.printf "upwards_begins: %s\n" (Tools.list_to_string (List.map string_of_int upwards_begins)); *)
  List.fold_left (fun seq beg_i ->
    Ghost.move_down_in_seq beg_i seq
  ) seq upwards_begins

(** <private>
    Moves all ends upwards, starting from upwardmost ones. *)
let move_all_ends_upwards (seq : trm) : trm =
  let error = "Ghost_pair.move_all_ends_upwards: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let ends = ref [] in
  let find_ends i instr =
    match Resource_trm.ghost_end_inv instr with
    | Some _ -> ends := i :: !ends;
    | None -> ()
  in
  Mlist.iteri find_ends instrs;
  let downwards_ends = List.rev !ends in
  (* Printf.printf "downwards_ends: %s\n" (Tools.list_to_string (List.map string_of_int downwards_ends)); *)
  List.fold_left (fun seq end_i ->
    Ghost.move_up_in_seq end_i seq
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
    match Resource_trm.ghost_begin_inv instr with
    | Some (gv, _) ->
      begins_stack := (gv, i, i_with_delete) :: !begins_stack;
    | None ->
      begin match Resource_trm.ghost_end_inv instr with
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
  Resources.ensure_computed ();
  Target.apply_at_target_paths minimize_all_on_seq tg;
  Scope.infer_var_ids (); (* FIXME: move up/down should avoid breaking scopes *)
  Resources.justif_correct "only changed ghost code"


(** <private>
    cf. [fission]. *)
let fission_at (mark_between : mark) (split_i : int) (seq : trm) : trm =
  let error = "Ghost_pair.fission_at: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let tl1, tl2 = Mlist.split split_i instrs in

  (* 1. Find all scope begins before split point. *)
  let begins_stack = ref [] in
  let find_begins instr =
    match Resource_trm.ghost_begin_inv instr with
    | Some gbi -> begins_stack := gbi :: !begins_stack;
    | None ->
      begin match Resource_trm.ghost_end_inv instr with
      | Some gv ->
        begin match !begins_stack with
        | (gv_beg, _) :: bs_rest when gv = gv_beg ->
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
  let end_begin (gv, _) = Resource_trm.ghost_end gv in
  let ends = List.map end_begin !begins_stack in

  (* 3. Re-open scopes after split point. *)
  let subst_after_split = ref Var_map.empty in
  let re_begin (gv, g_call) =
    let gv' = Resource_trm.generate_ghost_pair_var () in
    let g_beg = Resource_trm.ghost_begin gv' g_call in
    subst_after_split := Var_map.add gv (trm_var gv') !subst_after_split;
    g_beg
  in
  let begins = List.rev_map re_begin !begins_stack in
  let tl2' = Mlist.map (trm_subst !subst_after_split) tl2 in

  (* 4. Construct resulting sequence. *)
  trm_seq_helper ~annot:seq.annot ?loc:seq.loc [TrmMlist tl1; TrmList ends; Mark mark_between; TrmList begins; TrmMlist tl2']

(** Distributes the scope of ghost pairs at the targeted sequence interstice. *)
let%transfo fission ?(mark_between : mark = no_mark) (tg : target) : unit =
  Target.iter (fun p_before ->
    let (p_seq, split_i) = Path.extract_last_dir_before p_before in
    apply_at_path (fission_at mark_between split_i) p_seq
  ) tg;
  justif_correct "ghosts where successfully distributed"


let find_inverse (ghost_fn: trm) (res: resource_set option) =
  let open Xoption.OptionMonad in
  Pattern.pattern_match ghost_fn [
    Pattern.(trm_var !__) (fun ghost_fn ->
      let* res in
      let* ghost_spec = Var_map.find_opt ghost_fn res.fun_specs in
      let* inv = ghost_spec.inverse in
      Some (trm_var inv)
    );
    Pattern.(trm_apps2 (trm_var (var_eq Resource_trm.var_with_reverse)) (trm_fun_with_contract __ __ !__) (trm_fun !__ !__)) (fun fwd_contract bwd_args bwd_body ->
        Some (trm_fun bwd_args None bwd_body ~contract:(FunSpecContract (revert_fun_contract fwd_contract)))
    );
    Pattern.(trm_apps2 (trm_var (var_eq Resource_trm.var_with_reverse)) __ !__) (fun inv -> Some inv);
    Pattern.__ None
  ]

let without_inverse (ghost_fn: trm) =
  Pattern.pattern_match ghost_fn [
    Pattern.(trm_apps2 (trm_var (var_eq Resource_trm.var_with_reverse)) !__ __) (fun fwd -> fwd);
    Pattern.__ ghost_fn
  ]

let debug_intro = false

let intro_at ?(name: string option) ?(end_mark: mark = no_mark) (i: int) (t_seq: trm) : trm =
  let seq = trm_inv ~error:"Ghost_pair.intro_on: Expect a sequence" trm_seq_inv t_seq in
  let seq_before, ghost_begin, seq_after = Mlist.get_item_and_its_relatives i seq in
  let ghost_call = trm_inv ~error:"Ghost_pair.intro_on: Should target a ghost call" Resource_trm.ghost_inv ghost_begin in

  let invoc_linear_pre_and_post t =
    match t.ctx.ctx_resources_contract_invoc with
    | Some invoc when invoc.contract_produced.produced_pure <> [] -> None
    | Some invoc ->
      let pre = List.map (fun used -> (used.hyp, used.used_formula)) invoc.contract_inst.used_linear in
      let post = List.map (fun prod -> (prod.produced_hyp, prod.produced_formula)) invoc.contract_produced.produced_linear in
      Some (pre, post)
    | None -> failwith "No contract invocation on ghost call (resources might not have been computed)"
  in
  let begin_invoc_res = match invoc_linear_pre_and_post ghost_begin with
    | Some res -> res
    | None -> failwith "Cannot transform into ghost pair a ghost producing pure facts (not reversible)"
  in

  let are_inverse_invoc_res (pre1, post1) (pre2, post2) =
    let open Resource_computation in
    try
      let _ = subtract_linear_resource_set pre1 post2 in
      let _ = subtract_linear_resource_set pre2 post1 in
      true
    with Resource_not_found _ as exn ->
      if debug_intro then Printf.eprintf "%s\n\n" (Printexc.to_string exn);
      false
  in

  let exception FoundInverse of int * trm in
  try
    Mlist.iteri (fun i t ->
      match Resource_trm.ghost_inv t with
      | Some end_ghost_call ->
        let correct_mark = trm_add_mark_is_noop end_mark t in
        if correct_mark then
          begin match invoc_linear_pre_and_post t with
          | Some end_invoc_res when are_inverse_invoc_res begin_invoc_res end_invoc_res ->
            raise (FoundInverse (i, t))
          | _ -> ()
          end
      | None -> ()
    ) seq_after;
    failwith "No ghost candidate for forming the end of a pair"

  with FoundInverse (i, end_ghost) ->
    let is_reversible = Option.is_some (find_inverse ghost_call.ghost_fn ghost_begin.ctx.ctx_resources_before) in
    let _, ghost_begin, ghost_end = if is_reversible then
        Resource_trm.ghost_pair ?name ghost_call
      else
        Resource_trm.ghost_custom_pair ?name (trm_specialized_ghost_closure ghost_begin) (trm_specialized_ghost_closure ~remove_contract:true end_ghost)
    in
    let seq_after = Mlist.replace_at i ghost_end seq_after in
    let seq = Mlist.merge_list [seq_before; Mlist.of_list [ghost_begin]; seq_after] in
    trm_replace (Trm_seq seq) t_seq

(** Introduce a ghost pair starting on the targeted ghost, and ending at the first closing candidate. *)
let%transfo intro ?(name: string option) ?(end_mark: mark = no_mark) (tg: target) =
  Resources.ensure_computed ();
  Target.iter (fun p ->
    let i, p = Path.index_in_seq p in
    apply_at_path (intro_at ?name ~end_mark i) p
  ) tg;
  Resources.justif_correct "only changed ghost code"


let elim_at ?(mark_begin: mark = no_mark) ?(mark_end: mark = no_mark) (i: int) (t_seq: trm): trm =
  let seq = trm_inv ~error:"Ghost_pair.elim_on: Expect a sequence" trm_seq_inv t_seq in
  let seq_before, ghost_begin, seq_after = Mlist.get_item_and_its_relatives i seq in
  let pair_var, { ghost_fn; ghost_args } = trm_inv ~error:"Ghost_pair.elim_on: Should target a ghost_begin" Resource_trm.ghost_begin_inv ghost_begin in

  let exception FoundEnd of int in
  try
    Mlist.iteri (fun i t ->
      match Resource_trm.ghost_end_inv t with
      | Some var when var_eq var pair_var -> raise (FoundEnd i)
      | _ -> ()
    ) seq_after;
    failwith "Could not find a matching ghost_end"

  with FoundEnd i ->
    let inverse_ghost_fn = match find_inverse ghost_fn ghost_begin.ctx.ctx_resources_before with
      | Some inv -> inv
      | None -> failwith "Found a non reversible ghost pair"
    in

    let seq_after = Mlist.replace_at i (trm_add_mark mark_end (Resource_trm.ghost { ghost_fn = inverse_ghost_fn; ghost_args })) seq_after in
    let seq = Mlist.merge_list [seq_before; Mlist.of_list [trm_add_mark mark_begin (Resource_trm.ghost { ghost_fn = without_inverse ghost_fn; ghost_args })]; seq_after] in
    trm_replace (Trm_seq seq) t_seq

(** Split a ghost pair into two independant ghost calls *)
let%transfo elim ?(mark_begin: mark = no_mark) ?(mark_end: mark = no_mark) (tg: target) =
  Resources.ensure_computed ();
  Target.iter (fun p ->
    let i, p = Path.index_in_seq p in
    apply_at_path (elim_at ~mark_begin ~mark_end i) p
  ) tg;
  Resources.justif_correct "only changed ghost code"


(** [elim_all_pairs_at gen_mark p]: remove all the pairs inside the sequence pointed by the given path, generating marks to merge them back later.
  Returns the list of eliminated pair in their sequence order.
  In the returned list each ghost pair is represented by a triplet containing:
  - the variable name of the pair,
  - the generated mark on the ghost replacing the ghost_begin,
  - the generated mark on the ghost replace the ghost_end. *)
let elim_all_pairs_at (gen_mark: unit -> mark) (p: path): (var * mark * mark) list =
  Resources.ensure_computed ();
  let begin_target = [nbAny; Constr_paths [p]; cStrict; cVarDef ~body:[cCall "__ghost_begin"] ""] in
  let marks = ref [] in
  Target.iter (fun p ->
    let mark_begin = gen_mark () in
    let mark_end = gen_mark () in
    let t_let = resolve_path p in
    let _, pair_token, _, _ = trm_inv trm_let_inv t_let in
    marks := (pair_token, mark_begin, mark_end) :: !marks;
    let i, p = Path.index_in_seq p in
    apply_at_path (elim_at ~mark_begin ~mark_end i) p
  ) begin_target;
  List.rev !marks

(* LATER: Also add intro_all_pairs_at *)

(* Reintroduce pairs that correspond to the marks given *)
let%transfo reintro_pairs_at (pairs: (var * mark * mark) list) (p: path): unit =
  Resources.ensure_computed ();
  (* FIXME: Quadratic search of marks *)
  List.iter (fun (pair_token, begin_mark, end_mark) ->
    Target.iter (fun p ->
      let i, p = Path.index_in_seq p in
      apply_at_path (intro_at ~name:pair_token.name ~end_mark i) p
    ) [Constr_paths [p]; cMark begin_mark]
  ) pairs;
  Resources.justif_correct "only changed ghost code"

(* DEPRECATED:
      1) Ghost_pair.elim_all_pairs
      2) Loop.fission
      3) Ghost.embed_loop
      4) Ghost_pair.intro

let hoist_on (t_loop : trm) : trm =
  let error = "Ghost_pair.hoist_on: expected simple loop" in
  let (range, body, contract) = trm_inv ~error trm_for_inv t_loop in
  let instrs = trm_inv ~error trm_seq_inv body in

  let ghost_beg_stack = ref [] in
  let rec find_ghost_begs remaining_instrs =
    match remaining_instrs with
    | [] -> ()
    | t :: remaining_instrs ->
      match trm_ghost_begin_inv t with
      | Some (gv, g_call) ->
        ghost_beg_stack := (t, gv, g_call) :: !ghost_beg_stack;
        find_ghost_begs remaining_instrs
      | None -> ()
  in
  find_ghost_begs (Mlist.to_list instrs);
  if !ghost_beg_stack = [] then t_loop else begin
  let ghost_beg_instrs, other_instrs = Mlist.split (List.length !ghost_beg_stack) instrs in

  let ghost_to_inverse = ref Var_map.empty in
  let ghost_forward = List.rev_map (fun (ghost_begin, gv, { ghost_fn; ghost_args }) ->
    let s = Ast_to_text.default_style () in
    let error = Printf.sprintf "Ghost_pair.host_on: could not find inverse of %s" (Ast_to_text.ast_to_string ~style:{s with only_desc = true} ghost_fn) in
    (* printf "ghost_begin res: %s\n" (Xoption.to_string (fun r -> (Tools.document_to_string (AstC_to_c.resource_set_to_doc (AstC_to_c.default_style ()) r))) ghost_begin.ctx.ctx_resources_before); *)
    let inverse = unsome_or_trm_fail ghost_begin error (find_inverse ghost_fn ghost_begin.ctx.ctx_resources_before) in
    ghost_to_inverse := Var_map.add gv (trm_ghost { ghost_fn = inverse; ghost_args }) !ghost_to_inverse;
    trm_ghost { ghost_fn = without_inverse ghost_fn; ghost_args }
  ) !ghost_beg_stack in

  let ghost_backward_stack = ref [] in
  let instrs' = Mlist.filteri (fun _ instr ->
    match trm_ghost_end_inv instr with
    | Some gv when Var_map.mem gv !ghost_to_inverse ->
      let inverse = Var_map.find gv !ghost_to_inverse in
      ghost_backward_stack := inverse :: !ghost_backward_stack;
      false
    | _ -> true
  ) other_instrs in

  let wrap_in_for ghosts =
    trm_copy (trm_for_instrs range (Mlist.of_list ghosts))
  in
  let _, ghost_group_beg, ghost_group_end = trm_ghost_custom_pair
    (trm_fun [] (Some (typ_unit ())) (wrap_in_for ghost_forward))
    (trm_fun [] (Some (typ_unit ())) (wrap_in_for (List.rev !ghost_backward_stack)))
  in
  let res = trm_seq_nobrace_nomarks [
    ghost_group_beg;
    trm_for ~annot:t_loop.annot ?loc:t_loop.loc ?contract range (trm_seq instrs');
    ghost_group_end
  ] in
  Show.trm ~msg:"res" res;
  res
  end
*)
