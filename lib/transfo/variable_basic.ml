open Prelude


(** [fold ~at tg]: expects the target [tg] to point at a variable declaration,
      [at] - denotes a target where the folding is done. If empty the folding operation
             is performed on all the ast nodes in the same level as the
             declaration or deeper, by default [at] = []. *)
let%transfo fold ?(at : target = []) (tg : target) : unit =
  Target.apply_at_target_paths_in_seq (Variable_core.fold_decl_at at) tg

(** [unfold ~mark ~at tg]: expects the target [tg] to be pointing at a variable declaration,
    then it will replace all the occurence of this variable inside the terms pointed by the
    target [~at] by the variable definition.
      [mark] - a mark to be added everywhere the variable was unfolded,
      [at] - denotes a target where the unfolding is done. If empty the operation
            is performed on all the ast nodes in the same level as the
            targeted declaration or deeper, by default [at] = [],
*)
let%transfo unfold ?(mark : mark = no_mark) ~(at : target) (tg : target) : unit =
  Scope.infer_var_ids (); (* FIXME: This should be done by previous transfo instead *)
  Target.iter (fun p ->
    let t_decl = Target.resolve_path p in
    let x, _, init = trm_inv ~error:"Variable_core.unfold: expected a target to a variable definition" trm_let_inv t_decl in
    if !Flags.check_validity then begin
      if Resources.trm_is_pure init then
        (* Case 1: pure expression *)
        Trace.justif "inlining a pure expression is always correct"
      else
        failwith "not yet implemented: factorize validity check with inlining"
    end;
    let init = trm_add_mark mark init in
    Target.apply_at_target_paths (trm_subst_var x init) at
  ) tg

(** [inline ~mark ~at tg]: expects the target [tg] to be pointing at a variable declaration,
    then it will find all the occurrences of that variable and replace them with its initial value.
      [mark] - a mark to be added everywhere the variable was inlined,
      [delete_decl] - if true, then the declaration is removed during the inlining.
*)
let%transfo inline ?(delete_decl : bool = true) ?(mark : mark = no_mark) (tg : target) : unit =
  (* if !Flags.check_validity then Scope.infer_var_ids (); (* FIXME: This should be done by previous transfo instead *) *)
  if !Flags.use_resources_with_models then Resources.ensure_computed ();
  Target.iter (fun p ->
    let (p_seq, p_local, index) = Internal.get_instruction_in_surrounding_sequence p in
    assert (p_local = []);
    let error = "Variable_core.inline: expected the surrounding sequence." in
    Target.apply_at_path (fun t_seq ->
      let tl, result = trm_inv ~error trm_seq_inv t_seq in
      let dl = Mlist.nth tl index in
      let x, _, init = trm_inv ~error:"expected a target to a variable definition" trm_let_inv dl in
      if !Flags.use_resources_with_models then begin
        (* when using models, type-checking is sufficient to check for correctness *)
        let init = trm_add_mark mark init in
        let res = Resources.after_trm init in
        let init_model = trm_add_mark mark (Var_map.find Resource_set.var_result res.aliases) in
        (* Printf.printf "rs: %s\n" (Resource_computation.resource_set_to_string res);
        let init_model = (Option.unsome ~error:"expected init result" Resource_set.(find_pure var_result res)) in *)
        let rec perform_subst_formula (f: formula): formula =
          Pattern.pattern_match f [
            Pattern.(trm_specific_var x) (fun () -> trm_copy init_model);
            Pattern.(__) (fun () -> trm_map perform_subst_formula f)
          ] in
        let rec perform_subst_trm (t: trm): trm =
          Pattern.pattern_match t [
            Pattern.(trm_specific_var x) (fun () -> trm_copy init);
            Pattern.(__) (fun () -> trm_map ~f_formula:perform_subst_formula perform_subst_trm t)
          ] in
        let new_tl = Mlist.update_at_index_and_fix_beyond ~delete:delete_decl index (fun t -> t) perform_subst_trm tl in
        trm_seq ~annot:t_seq.annot ?result new_tl
      end else begin
      (* LEGACY: shapes *)
      if !Flags.check_validity then begin
        if Resources.trm_is_pure init then
          (* Case 1: pure expression *)
          Trace.justif "inlining a pure expression is always correct"
        else begin
          (* Case 2: duplicable expression can be inlined if we don't go through interfering context, control flow or formulas *)
          Resources.required_for_check ();
          (* Resources.assert_instr_effects_shadowed p; *)
          (* -- DUPLICATE CODE *)
          let t_seq = Target.resolve_path p_seq in
          let tl, _ = trm_inv ~error trm_seq_inv t_seq in
          let dl = Mlist.nth tl index in
          let x, _, init = trm_inv ~error:"expected a target to a variable definition" trm_let_inv dl in
          (* -- *)
          Resources.assert_not_self_interfering init;
          let occurences = Constr.resolve_target ~prefix:p_seq [nbMulti; cVarId x] t_seq in
          let end_occ_index = match snd (List.unlast occurences) with
          | Dir_seq_nth i :: _ -> i
          | p -> path_fail p "expected path to be inside current sequence"
          in
          (* check that we don't go through control flow or formulas *)
          List.iter (fun occ_p ->
            List.iter (fun dir ->
              let open Dir in
              match dir with
              | Dir_body | Dir_then | Dir_else
              | Dir_for_start | Dir_for_stop | Dir_for_step
              | Dir_for_c_init | Dir_for_c_step | Dir_case _
              | Dir_contract _ | Dir_ghost_arg_nth _ ->
                path_fail (p_seq @ occ_p) (sprintf "inlining non-pure expression does not support going through %s yet" (Dir.dir_to_string dir))
              | _ -> ()
            ) occ_p
          ) (List.drop 1 occurences);
          (* is calling this useful?
          Resources.assert_dup_instr_redundant index last_occ_index t_seq; *)
          let usage = Resources.usage_of_trm init in
          let _, instrs_after_let = Mlist.split (index + 1) tl in
          let context_instrs, _ = Mlist.split (end_occ_index - index - 1) instrs_after_let in
          (* DEBUG: Show.trms ~msg:"\n---- HERE:\n" (Mlist.to_list context_instrs); *)
          let context_usage = Resources.compute_usage_of_instrs context_instrs in
          (* TODO: double check that we don't need to check commute for every occ and not just last one. *)
          Resources.assert_usages_commute ~res_ctx:(Resources.after_trm init) [path_error_context p] usage context_usage;
          Trace.justif "inlining a duplicable expression through a non-interfering, non-control-flow and non-formula context is correct"
          (* TODO: Case 3 ? recursive traversal analysis with special constructor cases? *)
          (* trm_fail init "inlining non-pure expression is not yet supported, requires checking for interference similar to instr.swap, loop.move_out, etc" *)
        end
      end;
      let init = trm_add_mark mark init in
      let new_tl = Mlist.update_at_index_and_fix_beyond ~delete:delete_decl index (fun t -> t) (trm_subst_var x init) tl in
      trm_seq ~annot:t_seq.annot ?result new_tl
      end
    ) p_seq
  ) tg

(** [rename ~into tg]: expects the target [tg] to be pointing at a declaration, then it will
    rename its declaration and all its occurrences. *)
let%transfo rename ~into:(new_name : string) (tg : target) : unit =
  Trace.justif "correct if there is no name conflict (checked through variable ids)";
  Target.apply_at_target_paths_in_seq (Variable_core.rename_at new_name) tg

(** [init_detach tg]: expects the target [tg] to point at a variable initialization.
   It then splits the instruction into a variable declaration and a set operation. *)
let%transfo init_detach (tg : target) : unit =
  Trace.justif_always_correct ();
  Nobrace_transfo.remove_after ( fun _ ->
    Target.apply_at_target_paths (Variable_core.init_detach_on) tg
  )

(** [init_attach const tg]: expects the target [tg] to point at a variable declaration,
    DEPRECATED: Then it will search inside the sequence which contains the variable declaration
    for an unique assigment. Then it will replace that assignment with a new initialized
    variable declaration.
    INSTEAD: search immediately following set operation *)
let%transfo init_attach (tg : target) : unit =
  Trace.justif_always_correct ();
  Target.apply_at_target_paths_in_seq Variable_core.init_attach_at tg


(** [local_name_on mark curr_var var_typ local_var t] declares a local
  variable [local_var] and replaces [curr_var] with [local_var] in [t].
    - [curr_var]: the replaced variable
    - [var_typ]: the type of the variable
    - [local_var]: the name of the local variable to be declared
    - [t]: the modified term that contains [curr_var].
  *)
let local_name_on (curr_var : var) (var_typ : typ)
  ~(uninit_pre : bool) ~(uninit_post : bool)
  (local_var : string) (t : trm) : trm =
  let local_var = new_var local_var in
  let let_instr = trm_let_mut (local_var, var_typ) (trm_var_get ~typ:var_typ curr_var) in
  let set_instr = trm_set (trm_var ~typ:var_typ curr_var) (trm_var_get ~typ:var_typ local_var) in
  let new_t = trm_subst_var curr_var (trm_var local_var) t in
  trm_seq_nobrace_nomarks [let_instr; new_t; set_instr]

(** [local_name ~var var_typ ~local_var tg] declares a local
  variable [local_var] and replaces [var] with [local_var] in
  the term at target [tg].

  TODO: compose as Instr.insert alloc; Storage/Cell.reuse;
  *)
let%transfo local_name ~(var : var) (var_typ : typ)
  ?(uninit_pre : bool = false) ?(uninit_post : bool = false)
  ~(local_var : string) (tg : target) : unit =
  if (uninit_pre || uninit_post) then
    failwith "not implemented";
  Target.iter (fun p -> Marks.with_fresh_mark_on p (fun m ->
    Nobrace_transfo.remove_after (fun () ->
      Target.apply_at_path (local_name_on var var_typ ~uninit_pre ~uninit_post local_var) p
    );
    if !Flags.check_validity && not !Flags.preserve_specs_only then begin
      step_backtrack ~discard_after:true (fun () ->
        let p = resolve_mark_exactly_one m in
        Nobrace_transfo.remove_after (fun () ->
        Target.apply_at_path (fun t ->
          let (_, open_w, close_w) = Resource_trm.ghost_pair_hide
            (Resource_formula.formula_cell_var ~typ:var_typ var) in
          trm_seq_nobrace_nomarks [open_w; t; close_w]
        ) p
        );
        Resources.ensure_computed_at p
      )
    end
  )) tg

(** [delocalize array_size neutral_element fold_operation tg]: expects the target [tg] to point to
    a block of code of the following form
      T a

   { T x = a; // mandatory format for first instruction

      for (int i = ...)
         x++;

      a = x;  // mandatory format for last instruction
   }@nobrace then
   Then it will transform it into:
       T a

   {
      { T x[N];
         x[0] = a;
         for (k = 1; k < N; k++)
            x[k] = 0;
      }@nobrace

      for (int i = ...)
         a++;

      { a = 0;
         for (k = 1; k < N; k++)
            a = a + x[k];  // could be a += if exists
         }@nobrace

   }@nobrace.
   [index] - denotes the index of the two added loops
   [array_size] - denotes the size of the array inside the block
   [ops] - the delocalize operation, it can be an arithmetic delocalization or an object delocalization
    of the array declared inside the block. *)
let%transfo delocalize ?(index : string = "dl_k") ~(array_size : trm) ~ops:(dl_o : local_ops) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    Target.apply_at_target_paths (Variable_core.delocalize_at array_size dl_o index) tg)


(** [change_type new_type tg]: expects [tg] to point a variable declaration, then it will change the type of
    that variable with [new_type]. *)
let%transfo change_type (new_type : string) (tg : target) : unit =
 Target.apply_at_target_paths_in_seq (Variable_core.change_type_at new_type) tg


(** [insert ~constr ~name ~typ ~value tg]: expects the target [tg] to point at any relative location in a sequence
     then it will insert a variable declaration on that location,
     [const] - if true, then the inserted variable is going to be immutable, otherwise mutable,
     [reparse] - if true it will reparse the full ast after applying the transformation,
     [value] - initial value of the inserted variable,
     [name] - name of the inserted variable,
     [typ] - typ of the inserted variable;.

    NOTE: if initialization [value] is not provided then the declaration will be un-initialized. *)
let%transfo insert ?(const : bool = false) ?(reparse : bool = false) ~(name : string) ~(typ : typ) ?(value : trm option) (tg : target) : unit =
  Target.reparse_after ~reparse (Target.iter (fun p ->
    let (p_seq, i) = Path.extract_last_dir_before p in
    Target.apply_at_path (Variable_core.insert_at i const name typ value) p_seq;
    if !Flags.check_validity then begin (* NOTE: same as instruction insertion *)
      Resources.ensure_computed ();
      Resources.assert_instr_effects_shadowed (p_seq @ [Dir_seq_nth i]);
      Trace.justif "nothing modified by the instruction is observed later"
    end
  )) tg

(** [subst ~subst ~space tg]]: expects the target [tg] to point at any trm that could contain an occurrence of the
    variable [subst], then it will check for occurrences of the variable [subst] and replace is with [put]. *)
let%transfo subst ?(reparse : bool = false) ~(subst : var) ~(put : trm) (tg : target) : unit =
  Target.reparse_after ~reparse (
    Target.iter (fun p ->
      if !Flags.check_validity then begin
        let instr_p, expr_p = Path.path_in_instr p (Trace.ast ()) in
        Nobrace_transfo.remove_after (fun () -> (* FIXME: handle no brace in scope and typing to remove more lazily? *)
        Target.apply_at_path (fun instr_t ->
          let res_before = Resources.before_trm instr_t in
          let res_after = Resources.after_trm instr_t in
          let g = Resource_trm.may_ghost_intro_alias subst put res_before in
          let filter (_, f) = is_free_var_in_trm subst f in
          let res_filter = Resource_set.filter
            ~pure_filter:filter ~linear_filter:filter
            (*~aliases_filter:(fun _ _ -> false) ~spec_filter:(fun _ _ -> false)*) in
          let res_before_touched = res_filter res_before in
          let res_after_touched = res_filter res_after in
          let res_before_changed = Resource_set.subst_var subst put res_before_touched in
          let res_after_changed = Resource_set.subst_var subst put res_after_touched in
          (* NOTE: #equiv-rewrite
          these ghosts assume that all affected code typechecks with the substitution applied to the entire resource context, this is not always true, it might be necessary to forget the substitution on resources consumed by function calls, and learn the substitution on resources produced by function calls.
          This is a general problem for any equivalence rewrite, in particular for arithmetic simplification. *)
          let change_res_before = Resource_trm.ghost_admitted {
            pre = res_before_touched; post = res_before_changed
          } in
          let change_res_after = Resource_trm.ghost_admitted {
            pre = res_after_changed; post = res_after_touched
          } in
          let t = Path.apply_on_path (trm_subst_var subst put) instr_t expr_p in
          match trm_seq_inv t with
          | Some (instrs, result) ->
            (* is this code path used? *)
            trm_seq_helper ~annot:t.annot ?result [Trm g; Trm change_res_before; TrmMlist instrs; Trm change_res_after]
          | None ->
            trm_seq_helper ~braces:false [Trm g; Trm change_res_before; Trm t; Trm change_res_after]
        ) instr_p);
        Resources.justif_correct (sprintf "can substitute %s for its value" subst.name)
      end else
        Target.apply_at_path (trm_subst_var subst put) p
    )
  ) tg

(** <private> *)
let elim_analyse (xy : (var * var) option ref) (t : trm) : trm =
  let error = "expected variable declaration" in
  let (x, ty, init) = trm_inv ~error trm_let_inv t in (* instead detect if there is x = y *)
  assert (Option.is_some (trm_ref_inv init));
  let error = "expected initial value to be a ref(get(var))" in
  let (_ty, init_val) = trm_inv ~error trm_ref_inv init in
  let init_val_get = trm_inv ~error trm_get_inv init_val in
  let init_val_get_var = trm_inv ~error trm_var_inv init_val_get in
  xy := Some (x, init_val_get_var);
  t

(** <private> *)
(* Matches a copy instruction x = y *)
let elim_match_copy_back (t : trm) : (var * var) option =
  try
    let lhs, rhs = trm_inv (trm_binop_inv Binop_set) t in
    let x, init_val = trm_inv trm_var_inv lhs, rhs in
    let init_val_get = trm_inv trm_get_inv init_val in
    let init_val_get_var = trm_inv trm_var_inv init_val_get in
    Some (x, init_val_get_var)
  with _ -> None

(** <private> *)
let elim_reuse_on (i : int) (x : var) (y : var) (seq_t : trm) : trm =
  let error = "expected sequence" in
  let instrs, result = trm_inv ~error trm_seq_inv seq_t in
  let update_decl t =
    trm_seq_nobrace_nomarks []
  in
  let substitute_var t =
    (* if this is the copy-back instruction, delete it. If not, substitute x with y *)
    match elim_match_copy_back t with
    | Some (y', x') -> if x = x' && y = y' then trm_seq_nobrace_nomarks [] else trm_subst_var x (trm_var y) t
    | None -> trm_subst_var x (trm_var y) t
  in
  let new_instrs = Mlist.update_at_index_and_fix_beyond i ~delete:true
    update_decl substitute_var instrs
  in
  trm_seq ~annot:seq_t.annot ?loc:seq_t.loc ?result new_instrs

(** [elim_reuse]: given a targeted variable declaration [let x = get(y)], eliminates the variable
  declaration, reusing variable [y] instead of [x].
  This is correct if [y] is not used in the scope of [x], and can be uninit after the scope of [x].
  Additionally, it checks if there is a copy-back instruction [y = x], in which case it removes it.
  This only works if x is never used (or written into) after the copy-back.

  TODO: think about the relationship between this, [reuse], [elim_redundant], and [local_name].
  local_name should Instr.insert alloc; Storage.reuse; Storage.read_last_write; Instr.delete x = x
  *)
let%transfo elim_reuse (tg : target) : unit =
  Target.iter (fun p ->
    let xy = ref None in
    let _ = Path.apply_on_path (elim_analyse xy) (Trace.ast ()) p in
    let (x, y) = Option.get !xy in
    let (i, p_seq) = Path.index_in_seq p in
    if !Flags.check_validity then begin
      step_backtrack ~discard_after:true (fun () ->
        Target.apply_at_path (fun t_seq ->
          let error = "expected sequence" in
          let instrs, result = trm_inv ~error trm_seq_inv t_seq in
          let y_cell = Resource_formula.formula_uninit_cell_var y in
          let (_, open_hide, close_hide) = Resource_trm.ghost_pair_hide y_cell in
          (* detect if there is y = x at the end *)
          let copy_back_index = ref None in
          Mlist.iteri (fun i instr ->
            match elim_match_copy_back instr with
            | None -> ()
            | Some (y', x') -> if x' = x && y' = y then copy_back_index := Some i
          ) instrs;
          (* if we don't have a copy-back, we want to uninit y, because reading from it after the
            transformation should result in the value of x, unless y has been written into in the meantime *)
          (* if we do have a copy-back, we can read from y after the transformation, because it is guaranteed to have the value of x
            However, we also want to prevent any occurence of x after the copy-back, because y is allowed again and its value
           could be different than the value of x after a few instructions (this is a strong constraint, which could be relaxed) *)
          let instrs, end_trms = match !copy_back_index with
            | None ->
              let forget_init_y = Resource_trm.ghost_forget_init y_cell in
              instrs, [Trm forget_init_y]
            | Some i ->
              let instrs, end_trms = Mlist.split i instrs in
              let copy_back_instr, end_trms = Mlist.split 1 end_trms in
              let x_cell = Resource_formula.formula_uninit_cell_var x in
              let (_, open_x_hide, close_x_hide) = Resource_trm.ghost_pair_hide x_cell in
              (* during resource check, we keep the copy-back instr so that y can be read from after the transformed seq *)
              instrs, [TrmMlist copy_back_instr; Trm open_x_hide ; TrmMlist end_trms; Trm close_x_hide]
          in
          let before_instrs, instrs = Mlist.split (i + 1) instrs in
          trm_seq_helper ~annot:t_seq.annot ?result
            ([
              TrmMlist before_instrs; (* untouched instructions before the transfo *)
              Trm open_hide;          (* hide y *)
              TrmMlist instrs;        (* instructions where we will substitute x with y *)
              Trm close_hide          (* unhide y *)
            ] @ end_trms)
        ) p_seq;
        Resources.ensure_computed_at p_seq
      );
      Trace.justif (sprintf "variable %s is not used after declaration" y.name)
    end;
    Nobrace_transfo.remove_after (fun () ->
      Target.apply_at_path (elim_reuse_on i x y) p_seq)
  ) tg

(** [bind ~const ~mark fresh_name tg]: expects the target [tg] to be pointing at any trm, then it will insert a variable declaration
      with name [fresh_name] just before the instruction that contains the target [tg], and replace the targeted trm with an occurrence
      of the variable [fresh_name].
      [const] - if true the binded variable will be immutable, otherwise mutable,
      [mark_let] - an optional mark for the created instruction
      [mark_occ] - an optional mark for the introduced occurrences
      [mark_body] - mark used for marking the body of the targeted trm,
      [typ] - type of the binded variable, needed when the type can't be deducted from the targeted trm,
      [fresh_name] - name of the binded variable. *)
(* LATER: document the behavior of ${occ} in the case [tg] aims at multiple targets *)
(* LATER: it seems that a mark is introduced and not eliminated *)
(* FIXME: should handle only the case const = true and let a combi perform the transformation into a mutable variable. In any case const = false is the wrong default ! *)
let%transfo bind ?(const : bool = false) ?(mark_let : mark = no_mark) ?(mark_occ : mark = no_mark) ?(mark_body : mark = no_mark) ?(remove_nobrace: bool = true) ?(typ : typ option) (fresh_name : string) (tg : target) : unit =
  Resources.justif_correct "the extracted sub-expression is required by typing to use resources that do not interfere with the other sub-expressions";
  Nobrace_transfo.remove_after ~remove:remove_nobrace (fun _ ->
    Target.iteri (fun occ p ->
      let p, p_local, i = Internal.get_instruction_in_surrounding_sequence p in
      let fresh_name = Tools.string_subst "${occ}" (string_of_int occ) fresh_name in
      Target.apply_at_path (Variable_core.bind_at ~mark_let ~mark_occ ~mark_body i fresh_name ~const ?typ p_local) p
    ) tg
  )

(** [to_const tg]: expects the target [tg] to be point at a variable declaration, then it will search inside
      the same scope if there are any write operations on that variable.
      If that's the case then the tranformation will fail(for safety reasons).
      Otherwise, first switch the mutability of that variable and then replace all get operations on that variable with its intialization
      value.
  @correctness: always correct if the result type checks. *)
let%transfo to_const (tg : target) : unit =
  Target.apply_at_target_paths_in_seq Variable_core.to_const_at tg;
  Resources.justif_correct "always correct if the result type checks"

(** [to_nonconst tg]: expects the target [tg] to be point at a variable declaration,
      If the variable is mutable then does nothing, otherwise change the mutability of the targeted variable to a mutable one,
      and replace all the variable occurrences with a get operation containing that occurrence. *)
let%transfo to_nonconst (tg : target) : unit =
  Target.apply_at_target_paths_in_seq Variable_core.to_nonconst_at tg


(** [simpl_deref ~indepth tg]: expects the target [tg] to be pointing at expressions of the form  *(&b), &( *b) in depth
    if [indepth] is set to true or at the give target if [indepth] is false.*)
let%transfo simpl_deref ?(indepth : bool = false) (tg : target) : unit =
  Trace.tag "simpl";
  Trace.justif_always_correct ();
  Target.apply_at_target_paths (Variable_core.simpl_deref_on indepth) tg

(** [exchange var1 var2 tg]: expects the target [tg] to point at an instruction that contains both the
    variable [var1] and [var2], then it will try to swap all the occurrences of [var1] with [var2]. *)
let%transfo exchange (v1 : var) (v2 : var) (tg : target) : unit =
  let tm = Var_map.empty in
  let tm = Var_map.add v1 (trm_var v2) tm in
  let tm = Var_map.add v2 (trm_var v1) tm in
  Target.apply_at_target_paths (fun t1 -> trm_subst tm t1) tg

(** [ref_to_pointer tg]: expects thee target [tg] to be pointing at a reference declaration, then it will convert
    this reference into a pointer. *)
let%transfo ref_to_pointer (tg : target) : unit =
  Target.apply_at_target_paths_in_seq (Variable_core.ref_to_pointer_at) tg

(** [ref_to_var tg]: expects the target [tg] to point at a refernce declaration,
     then it will convert it into a simple variable declaration. *)
let%transfo ref_to_var (tg : target) : unit =
  apply_at_target_paths (Variable_core.ref_to_var_on) tg
