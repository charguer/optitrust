open Prelude


(* [fold ~at tg]: expects the target [tg] to point at a variable declaration,
      [at] - denotes a target where the folding is done. If empty the folding operation
             is performed on all the ast nodes in the same level as the
             declaration or deeper, by default [at] = []. *)
let%transfo fold ?(at : target = []) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Variable_core.fold at i t p) tg

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
    let _, x, _, init = trm_inv ~error:"Variable_core.unfold: expected a target to a variable definition" trm_let_inv t_decl in
    let init = trm_add_mark mark init in
    Target.apply_at_target_paths (trm_subst_var x init) at
  ) tg

(** [inline ~mark ~at tg]: expects the target [tg] to be pointing at a variable declaration,
    then it will find all the occurrences of that variable and replace them with its initial value.
      [mark] - a mark to be added everywhere the variable was inlined,
      [delete_decl] - if true, then the declaration is removed during the inlining.
*)
let%transfo inline ?(delete_decl : bool = true) ?(mark : mark = no_mark) (tg : target) : unit =
  Scope.infer_var_ids (); (* FIXME: This should be done by previous transfo instead *)
  Target.iter (fun p ->
    let (p_seq, p_local, index) = Internal.get_instruction_in_surrounding_sequence p in
    let error = "Variable_core.unfold_aux: expected the surrounding sequence." in
    Target.apply_at_path (fun t_seq ->
      let t_seq = Target.resolve_path p_seq in
      let tl = trm_inv ~error trm_seq_inv t_seq in
      let dl = Mlist.nth tl index in
      let dl = Path.resolve_path p_local dl in
      let _, x, _, init = trm_inv ~error:"Variable_core.unfold: expected a target to a variable definition" trm_let_inv dl in
      if !Flags.check_validity then begin
        if Resources.trm_is_pure init then
          Trace.justif "inlining a pure expression is always correct"
        else
          trm_fail init "inlining non-pure expression is not yet supported, requires checking for interference similar to instr.swap, loop.move_out, etc"
      end;
      let init = trm_add_mark mark init in
      let new_tl = Mlist.update_at_index_and_fix_beyond ~delete:delete_decl index (fun t -> t) (trm_subst_var x init) tl in
      trm_seq ~annot:t_seq.annot new_tl
    ) p_seq
  ) tg

(* [rename ~into tg]: expects the target [tg] to be pointing at a declaration, then it will
    rename its declaration and all its occurrences. *)
let%transfo rename ~into:(new_name : string) (tg : target) : unit =
  Trace.justif "correct if there is no name conflict (checked through variable ids)";
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Variable_core.rename new_name i t p) tg

(* [init_detach tg]: expects the target [tg] to point at a variable initialization.
   It then splits the instruction into a variable declaration and a set operation. *)
let%transfo init_detach (tg : target) : unit =
  Trace.justif_always_correct ();
  Nobrace_transfo.remove_after ( fun _ ->
    Target.apply_on_targets (Variable_core.init_detach) tg
  )

(* [init_attach const tg]: expects the target [tg] to point at a variable declaration,
    Then it will search inside the sequence which contains the variable declaration
    for an unique assigment. Then it will replace that assignment with a new initialized
    variable declaration.
    [const] -denotes a booleean to decide if the new declaration is constant or not. *)
let%transfo init_attach ?(const : bool = false) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Variable_core.init_attach const i t p ) tg


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
  let local_var = Trm.new_var local_var in
  let let_instr = trm_let_mut (local_var, var_typ) (trm_var_possibly_mut ~typ:var_typ curr_var) in
  let set_instr = trm_set (trm_var ~typ:var_typ curr_var) (trm_var_possibly_mut ~typ:var_typ local_var) in
  let new_t = trm_subst_var curr_var (trm_var local_var) t in
  trm_seq_nobrace_nomarks [let_instr; new_t; set_instr]

(** [local_name ~var var_typ ~local_var tg] declares a local
  variable [local_var] and replaces [var] with [local_var] in
  the term at target [tg]. *)
let%transfo local_name ~(var : var) (var_typ : typ)
  ?(uninit_pre : bool = false) ?(uninit_post : bool = false)
  ~(local_var : string) (tg : target) : unit =
  if (uninit_pre || uninit_post) then
    failwith "not implemented";
  Target.iter (fun p -> Marks.with_fresh_mark_on p (fun m ->
    Nobrace_transfo.remove_after (fun () ->
      Target.apply_at_path (local_name_on var var_typ ~uninit_pre ~uninit_post local_var) p
    );
    if !Flags.check_validity then begin
      step_backtrack ~discard_after:true (fun () ->
        let p = resolve_mark_exactly_one m in
        Nobrace_transfo.remove_after (fun () ->
        Target.apply_at_path (fun t ->
          let (_, open_w, close_w) = Resource_trm.ghost_pair_hide
            (Resource_formula.formula_cell var) in
          trm_seq_nobrace_nomarks [open_w; t; close_w]
        ) p
        );
        recompute_resources ()
      )
      (* DEPRECATED:
      Resources.ensure_computed ();
      let t = get_trm_at_exn [cMark m] in
      let t_res_usage = Resources.usage_of_trm t in
      let t_res_before = Resources.before_trm t in
      let t_res_after = Resources.after_trm t in
      let used_formulas = Resources.(formulas_of_hyps (hyps_of_usage t_res_usage) (t_res_before.linear @ t_res_after.linear)) in
      let used_vars = List.fold_left (fun vs t ->
        Var_set.union vs (trm_free_vars t)
      ) Var_set.empty used_formulas in
      let var_conflicts = match Var_map.find_opt var t_res_before.aliases with
        | Some var_alias -> trm_free_vars var_alias
        | None -> Var_set.singleton var
      in
      if Var_set.disjoint var_conflicts used_vars then
        Trace.justif "resources do not mention replaced variable after transformation"
      else
        trm_fail t "resources still mention replaced variable after transformation"
      *)
    end
  )) tg

(* [delocalize array_size neutral_element fold_operation tg]: expects the target [tg] to point to
    a block of code of the following form
      T a

   { T x = a; // mendatory format for first instruction

      for (int i = ...)
         x++;

      a = x;  // mendatory format for last instruction
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
    Target.apply_on_targets (Variable_core.delocalize array_size dl_o index ) tg)


(* [change_type new_type tg]: expects [tg] to point a variable declaration, then it will change the type of
    that variable with [new_type]. *)
let%transfo change_type (new_type : typvar) (tg : target) : unit =
 Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p, i) -> Variable_core.change_type new_type i t p) tg


(* [insert ~constr ~name ~typ ~value tg]: expects the target [tg] to point at any relative location in a sequence
     then it will insert a variable declaration on that location,
     [const] - if true, then the inserted variable is going to be immutable, otherwise mutable,
     [reparse] - if true it will reparse the full ast after applying the trasnformation,
     [value] - initial value of the inserted variable,
     [name] - name of the inserted variable,
     [typ] - typ of the inserted variable;.

    NOTE: if initialization [value] is not provided then the declaration will be un-initialized. *)
let%transfo insert ?(const : bool = false) ?(reparse : bool = false) ?(value : trm = trm_lit (Lit_uninitialized)) ~name:(name : string) ~typ:(typ : typ) (tg : target) : unit =
  Target.reparse_after ~reparse (Target.apply_on_targets_between (fun t (p,i) -> Variable_core.insert i const name typ value t p)) tg


(* [subst ~subst ~space tg]]: expects the target [tg] to point at any trm that could contain an occurrence of the
    variable [subst], then it will check for occurrences of the variable [subst] and replace is with [put]. *)
let%transfo subst ?(reparse : bool = false) ~(subst : var) ~(put : trm) (tg : target) : unit =
  (* TODO: justif: needs to check that [subst = put] fact exists. *)
  Target.reparse_after ~reparse (
    Target.apply_on_targets (Variable_core.subst subst put)
  ) tg

(* [bind ~const ~mark fresh_name tg]: expects the target [tg] to be pointing at any trm, then it will insert a variable declaration
      with name [fresh_name] just before the instruction that contains the target [tg], and replace the targeted trm with an occurrence
      of the variable [fresh_name].
      [const] - if true the binded variable will be immutable, otherwise mutable,
      [mark_let] - an optional mark for the created instruction
      [mark_occ] - an optional mark for the introduced occurrences
      [mark_body] - mark used for marking the body of the targeted trm,
      [typ] - type of the binded variable, needed when the type can't be deducted from the targeted trm,
      [fresh_name] - name of the binded variable. *)
      (* LATER: document the behavior of ${occ} in the case [tg] aims at multiple targets *)
      (* LATER: document the [îs_ptr] and explain why it is needed *)
      (* LATER: it seems that a mark is introduced and not eliminated *)
let%transfo bind ?(const : bool = false) ?(mark_let : mark = no_mark) ?(mark_occ : mark = no_mark) ?(mark_body : mark = no_mark) ?(is_ptr : bool = false) ?(remove_nobrace: bool = true) ?(typ : typ option) (fresh_name : string) (tg : target) : unit =
  Resources.justif_correct "the extracted sub-expression is required by typing to use resources that do not interfere with the other sub-expressions";
  Nobrace_transfo.remove_after ~remove:remove_nobrace ( fun _ ->
    Target.applyi_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
    (fun occ  t (p, p_local, i) ->
      let fresh_name = Tools.string_subst "${occ}" (string_of_int occ) fresh_name in
      Variable_core.bind mark_let mark_occ mark_body i fresh_name const is_ptr typ p_local t p) tg;
  )

(* [to_const tg]: expects the target [tg] to be point at a variable declaration, then it will search inside
      the same scope if there are any write operations on that variable.
      If that's the case then the tranformation will fail(for safety reasons).
      Otherwise, first switch the mutability of that variable and then replace all get operations on that variable with its intialization
      value.
  @correctness: always correct if the result type checks. *)
let%transfo to_const (tg : target) : unit =
  Trace.justif "always correct if the result type checks (TODO: check)";
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
     ( fun t (p, i) -> Variable_core.from_to_const true i t p) tg

(* [to_nonconst tg]: expects the target [tg] to be point at a variable declaration,
      If the variable is mutable then does nothing, otherwise change the mutability of the targeted variable to a mutable one,
      and replace all the variable occurrences with a get operation containing that occurrence. *)
let%transfo to_nonconst (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
     ( fun t (p, i) -> Variable_core.from_to_const false i t p) tg


(* [simpl_deref ~indepth tg]: expects the target [tg] to be pointing at expressions of the form  *(&b), &( *b) in depth
    if [indepth] is set to true or at the give target if [indepth] is false.*)
let%transfo simpl_deref ?(indepth : bool = false) (tg : target) : unit =
  Trace.tag "simpl";
  Trace.justif_always_correct ();
  Target.apply_on_targets (Variable_core.simpl_deref indepth) tg

(* [exchange var1 var2 tg]: expects the target [tg] to point at an instruction that contains both the
    variable [var1] and [var2], then it will try to swap all the occurrences of [var1] with [var2]. *)
let%transfo exchange (v1 : var) (v2 : var) (tg : target) : unit =
  let tm = Var_map.empty in
  let tm = Var_map.add v1 (trm_var v2) tm in
  let tm = Var_map.add v2 (trm_var v1) tm in
  Target.apply_on_targets (
    Target.apply_on_path (fun t1 -> trm_subst tm t1)) tg

(* [ref_to_pointer tg]: expects thee target [tg] to be pointing at a reference declaration, then it will convert
    this reference into a pointer. *)
let%transfo ref_to_pointer (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p, i) -> Variable_core.ref_to_pointer i t p) tg

(* [ref_to_var tg]: expects the target [tg] to point at a refernce declaration,
     then it will convert it into a simple variable declaration. *)
let%transfo ref_to_var (tg : target) : unit =
  apply_on_targets (Variable_core.ref_to_var) tg
