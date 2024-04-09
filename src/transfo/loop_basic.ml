open Prelude
open Target
open Matrix_trm

(* [color nb_colors i_color tg]: expects the target [tg] to point at a simple for  loop,
   let's say [for (int i = start; i < stop; i += step) { body } ].
   [nb_colors] - an expression denoting the number of colors (e.g., ["2"]),
   [index] - denotes a fresh name to use as index for iterating over colors.

   In case [step = 1]:
   [for (int index = 0; index < nb_color; index++) {
      for (int i = index; i < stop; i += nb_color) { body }].

   In the general case, it produces:
   [for (int index = 0; index < nb_color; index++) {
      for (int i = index*step; i < stop; i += step*nb_color) { body }]. *)
let%transfo color (nb_colors : trm) ?(index : string option) (tg : target) : unit =
  apply_on_targets (Loop_core.color nb_colors index) tg

(* [tile tile_size index tg]: expects the target [tg] to point at a simple loop,
   say [for (int i = start; i < stop; i += step) { body } ].
   divides - denotes a flag to know if tile_size divides the size of the array or not
   [tile_size] - denotes the width of the tile (e.g., ["2"])
   [index] - denotes a fresh name to use as index for iterating over tiles.
   [bound] - can be one of
      - TileBoundMin: generates a constraint of the form  [i < min(X, bx+B)]
      - TileBoundAnd: generates a constraint of the form [i <  X && i < bx+B]
      - TileDivides: generates a constraint of the form [i < X], which is only true if B divides X

   It produces:
   [for (int index = 0; index < stop; index += tile_size) {
      for (int i = index; i < min(X, bx+B); i++) { body }]. *)
let%transfo tile ?(index : string = "b${id}")
         ?(bound : tile_bound = TileBoundMin)
         (tile_size : trm) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
    Target.apply_at_target_paths (Loop_core.tile index bound tile_size) tg
  )

(** <private> *)
let collapse_analyse (ri_rj_body : (loop_range * loop_contract * loop_range * loop_contract * trm) option ref) (t : trm) : trm =
  let error = "expected 2 nested simple loops" in
  let (ranges, body) = trm_inv ~error (trm_fors_inv 2) t in
  let (ri, ci) = List.nth ranges 0 in
  let (rj, cj) = List.nth ranges 1 in
  if not (is_trm_int 0 ri.start) || not (is_trm_int 0 rj.start) then
    trm_fail t "non-zero range starts are not yet supported: use loop shift first";
  if not (is_step_one (ri.step)) || not (is_step_one (rj.step)) then
    trm_fail t "non-unary range steps are not yet supported: use loop scale first";
  if ri.direction <> DirUp || rj.direction <> DirUp then
    trm_fail t "non-increasing range directions are not yet supported: use loop reverse first";
  ri_rj_body := Some (ri, ci, rj, cj, body);
  t

let ghost_group_collapse = toplevel_var "group_collapse"
let ghost_group_uncollapse = toplevel_var "group_uncollapse"
let ghost_group_collapse_ro = toplevel_var "group_collapse_ro"
let ghost_group_uncollapse_ro = toplevel_var "group_uncollapse_ro"
let ghost_group_collapse_uninit = toplevel_var "group_collapse_uninit"
let ghost_group_uncollapse_uninit = toplevel_var "group_uncollapse_uninit"

(** <private> *)
let collapse_on (simpl_mark : mark) (index : string)
  (ri : loop_range) (ci : loop_contract) (rj : loop_range) (cj : loop_contract)
  (body : trm) (t : trm) : trm =
  let nbi = ri.stop (* start = 0; trm_sub ri.stop ri.start *) in
  let nbj = rj.stop (* start = 0; trm_sub rj.stop rj.start *) in
  let k = new_var (index |>
    Tools.string_subst "${i}" ri.index.name |>
    Tools.string_subst "${j}" rj.index.name) in
  let var_k = trm_var ~typ:(typ_int ()) k in
  let rk = {
    index = k; start = trm_int 0; stop = trm_add_mark simpl_mark (trm_mul nbi nbj);
    direction = DirUp; step = Post_inc
  } in
  let new_i = trm_add_mark simpl_mark (* (trm_add ri.start = 0 *) (trm_div var_k nbj) in
  let new_j = trm_add_mark simpl_mark (* (trm_add rj.start = 0 *) (trm_mod var_k nbj) in
  let subst = Var_map.(empty |> add ri.index new_i |> add rj.index new_j) in
  (* TODO: works when invariants are the same, and pre/post are the same modulo a star,
    still need to insert ghosts to flatten the double star. *)
  (* group_collapse / group_uncollapse*)
  let open Resource_formula in
  let add_collapse_ghost ghost ghost_ro ghost_uninit =
    List.map (fun (_, formula) ->
      let ghost, formula = match formula_mode_inv formula with
      | Full, f -> ghost, f
      | RO, f -> ghost_ro, f
      | Uninit, f -> ghost_uninit, f
      in
      let items = trm_copy (formula_fun [ri.index, typ_int (); rj.index, typ_int ()] None formula) in
      Resource_trm.ghost (ghost_call ghost [
        "n", nbi; "m", nbj; "items", items
      ])
    )
  in
  let ghosts_before = add_collapse_ghost ghost_group_collapse ghost_group_collapse_ro ghost_group_collapse_uninit cj.iter_contract.pre.linear in
  let ghosts_after = add_collapse_ghost ghost_group_uncollapse ghost_group_uncollapse_ro ghost_group_uncollapse_uninit cj.iter_contract.post.linear in
  let contract = Resource_contract.loop_contract_subst subst cj in
  let body2 = if !Flags.check_validity then
    let instrs = trm_inv ~error:"expected seq" trm_seq_inv body in
    let open Resource_formula in
    let open Resource_trm in
    let instrs2 = instrs |>
      Mlist.push_front (assume (formula_in_range new_j (formula_loop_range rj))) |>
      Mlist.push_front (assume (formula_in_range new_i (formula_loop_range ri)))
    in
    trm_seq ~annot:body.annot instrs2
  else
    body
  in
  let t2 = trm_for ~contract rk (trm_subst subst body2) in
  if !Flags.check_validity then begin
    Resource_formula.(Resource_trm.(trm_seq_helper ~braces:false [
      Trm (assume (formula_geq ri.stop (trm_int 0)));
      Trm (assume (formula_geq rj.stop (trm_int 0)));
      TrmList ghosts_before;
      Trm t2;
      TrmList ghosts_after
    ]))
  end else
    t2

(** [collapse]: expects the target [tg] to point at a simple loop nest:
    [for i in 0..Ni { for j in 0..Nj { b(i, j) } }]
    And collapses the loop nest, producing:
    [for k in 0..(Ni*Nj) { b(k / Nj, k % Nj) } ]

    Correct if Ni >= 0 and Nj >= 0.

    This is the opposite of [tile].

    LATER:
    - could generate binders for i, j
    - could generate i := PROJ1(Ni, Nj, k), j := PROJ2(Ni, Nj, k)
    - then can simplify MINDEX2(Ni, Nj, PROJ1(Ni, Nj, k), PROJ2(Ni, Nj, k))
      |--> MINDEX2PROJ(Ni, Nj, k) |-- elim_mops --> k
    - can be generalized to collapsing N loops and deriving PROJNM and MINDEXNPROJ
    *)
let%transfo collapse ?(simpl_mark : mark = no_mark)
  ?(index : string = "${i}${j}") (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
  Target.iter (fun p ->
    let ri_rj_body = ref None in
    let _ = Path.apply_on_path (collapse_analyse ri_rj_body) (Trace.ast ()) p in
    let (ri, ci, rj, cj, body) = Option.get !ri_rj_body in
    if !Flags.check_validity then begin
    (* DEPRECATED: using assume instead
      step_backtrack ~discard_after:true (fun () ->
          Target.apply_at_path (fun t ->
            let contract = { pre = Resource_set.make ~pure (); post = Resource_set.empty } in
            let g = Resource_trm.ghost (ghost_closure_call contract (trm_seq_nomarks [])) in
            trm_seq_nobrace_nomarks [g; t]
          ) p;
          recompute_resources ()
        )
      );
    *)
      Trace.justif "correct when start >= stop for both ranges"
    end;
    Target.apply_at_path (collapse_on simpl_mark index ri ci rj cj body) p
  ) tg)

(* [hoist x_step tg]: expects [tg] to point at a variable declaration inside a
    simple loop. Let's say for {int i ...} {
        int x; [tg]
        ...
        x = ..
      }
    The targeted declaration should be detached, then the transformation it's going to introduce
    an array declaration right before the for loop that contains the targeted declaration.
    The declared array will have name [name], type the same as the one targeted by [tg] and the size
    of the array it's going to be equal to the [loop_bound -1]. All the variable occurrences are
    going to be replaced with array accesses at index the index of the for loop.

    [x_step] - denotes the array name that is going to hoist all the values of the targeted variable
    for each index of the for loop. *)
(* LATER/ deprecated
let hoist_old ?(name : string = "${var}_step") ?(array_size : trm option) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
    apply_on_transformed_targets (Path.index_in_surrounding_loop)
     (fun t (i, p) -> Loop_core.hoist_old name i array_size t p) tg)
*)
(* TODO: clean up code *)
let hoist_on (name : string)
             (mark_alloc : mark) (mark_free : mark) (mark_tmp_var : mark)
             (arith_f : trm -> trm)
             (decl_index : int) (t : trm) : trm =
  let error = "Loop_basic.hoist_on: only simple loops are supported" in
  let (range, body, contract) = trm_inv ~error trm_for_inv t in
  let { index; start; direction; stop; step} = range in
  assert (direction = DirUp); (* TODO: other directions *)
  let (array_size, new_index) = match step with
  | Pre_inc | Post_inc ->
     (trm_sub stop start, trm_sub (trm_var index) start)
  | Step s ->
    (* i = start; i < stop; i += step *)
    let trm_ceil_div a b =
      trm_div (trm_add a (trm_sub b (trm_int 1))) b
    in
     (trm_ceil_div (trm_sub stop start) s,
      trm_div (trm_sub (trm_var index) start) s)
  | _ -> trm_fail t "Loop_basic.hoist_on: unsupported loop step"
  in
  let body_instrs = trm_inv ~error trm_seq_inv body in
  let elem_ty = ref (typ_auto()) in
  let old_var = ref dummy_var in
  let new_var = ref dummy_var in
  let new_dims = ref [] in
  let update_decl (decl : trm) : trm =
    let error = "Loop_basic.hoist_on: expected variable declaration with MALLOCN initialization" in
    let (x, dims, etyp, elem_size) = trm_inv ~error Matrix_core.let_alloc_inv_with_ty decl in
    old_var := x;
    new_var := Trm.new_var (Tools.string_subst "${var}" x.name name);
    elem_ty := etyp;
    new_dims := (arith_f array_size) :: dims;
    let partial_indices = (arith_f new_index) ::
      (List.init (List.length dims) (fun _ -> trm_lit (Lit_int 0))) in
    let mindex = mindex !new_dims partial_indices in
    let new_access = trm_array_access (trm_var !new_var) mindex in
    let tmp_var = trm_let Var_immutable (x, typ_const_ptr etyp) new_access in
    trm_add_mark mark_tmp_var tmp_var
  in
  let body_instrs_new_decl = Mlist.update_nth decl_index update_decl body_instrs in
  let new_body_instrs = begin
    let free_index_opt = ref None in
    Mlist.iteri (fun i instr ->
      match Matrix_trm.free_inv instr with
      | Some freed ->
        begin match trm_var_inv freed with
        | Some freed_var when var_eq freed_var !old_var ->
          assert (Option.is_none !free_index_opt);
          free_index_opt := Some i;
        | _ -> ()
        end
      | _ -> ()
    ) body_instrs_new_decl;
    match !free_index_opt with
    | Some free_index -> Mlist.remove free_index 1 body_instrs_new_decl
    | None -> trm_fail body "Loop_basic.hoist: expected free instruction"
  end in
  let new_body_instrs, new_contract =
  if contract.strict then
    let dims = List.tl (!new_dims) in
    let other_indices = List.init (List.length dims) (fun _ -> Trm.new_var (fresh_var_name ())) in
    let indices = (arith_f new_index) :: (List.map trm_var other_indices) in
    let mindex = mindex !new_dims indices in
    let access = trm_array_access (trm_var !new_var) mindex in
    let grouped_access = List.fold_right (fun (i, d) acc ->
      (* FIXME: need to match inner loop ranges. *)
      Resource_formula.formula_group_range { index = i; start = trm_int 0; direction = DirUp; stop = d; step = Post_inc } acc
    ) (List.combine other_indices dims) Resource_formula.(formula_model access trm_cell) in
    let new_resource = Resource_formula.(formula_uninit grouped_access) in
    new_body_instrs, Resource_contract.push_loop_contract_clause (Exclusive Modifies) (Resource_formula.new_anon_hyp (), new_resource) contract
  else
    new_body_instrs, contract
  in
  let new_body = trm_seq ~annot:body.annot new_body_instrs in
  trm_seq_nobrace_nomarks [
    trm_add_mark mark_alloc
      (Matrix_core.let_alloc_with_ty !new_var !new_dims !elem_ty);
    trm_for ~contract:new_contract ~annot:t.annot range new_body;
    trm_add_mark mark_free
      (Matrix_trm.free !new_dims (trm_var !new_var));
  ]

(* TODO: document *)
let%transfo hoist ?(name : string = "${var}_step")
          ?(mark_alloc : mark = no_mark)
          ?(mark_free : mark = no_mark)
          ?(mark_tmp_var : mark = no_mark)
          ?(arith_f : trm -> trm = Arith_core.(simplify true gather_rec))
         (tg : target) : unit =
  Trace.justif_always_correct ();
  Nobrace_transfo.remove_after (fun _ ->
    Target.iter (fun p_instr ->
      let (i, p) = Path.index_in_surrounding_loop p_instr in
      apply_at_path (hoist_on name mark_alloc mark_free mark_tmp_var arith_f i) p
      ) tg)

(* [fission_on_as_pair]: split loop [t] into two loops

    [index]: index of the splitting point
    [t]: ast of the loop
    *)
let fission_on_as_pair (mark_loops : mark) (index : int) (t : trm) : trm * trm =
  let (l_range, t_seq, contract) = trm_inv
    ~error:"Loop_basic.fission_on: only simple loops are supported"
    trm_for_inv t
  in
  let tl = trm_inv trm_seq_inv t_seq in
  let tl1, tl2 = Mlist.split index tl in
  let fst_contract, snd_contract =
    if not !Flags.check_validity then
      empty_loop_contract, empty_loop_contract
    else
      let open Resource_formula in

      if not contract.strict then trm_fail t "Loop_basic.fission_on: requires a strict loop contract to check validity";

      (*
        for C(onsume) P(roduce) I(nvariant)
          tl1
          --- R = R' * I (= tl1.res_after = tl2.res_before)
          tl2

        no interfence <=> I = I' * I'' * Iro
        I' = I inter usageWrite(tl1)
          -> inter by hyp names
        Iro = I inter usageRO(tl1)
          -> inter by hyp names
        I'' = I \ (I' * Iro)
          -> using formulas

        R' = R \ I
          -> if using formulas

        for C R' (I' * Iro)
          tl1

        for R' P (I'' * RO(Iro))
          tl2
        *)

      let linear_invariant = contract.invariant.linear in (* = I *)
      let linear_invariant_hyps = Var_set.of_list (List.map (fun (h, _) -> h) linear_invariant) in

      let loop_start_res = Resources.before_trm t_seq in
      let tl1_usage = Resources.compute_usage_of_instrs tl1 in
      let tl1_inv_usage = (* = I' * Iro *)
        Hyp_map.filter (fun h _ -> Var_set.mem h linear_invariant_hyps) tl1_usage
      in
      let tl1_inv_reads, (* = Iro *) tl1_inv_writes (* = I' *) = Hyp_map.partition (fun _ res_usage ->
        match res_usage with
        | SplittedFrac | JoinedFrac -> true
        | _ -> false
      ) tl1_inv_usage in
      let resource_set_of_hyp_map (hyps: 'a Hyp_map.t) (resources: resource_item list): resource_item list =
        List.filter (fun (h, _) -> Hyp_map.mem h hyps) resources
      in
      let tl1_inv_reads = resource_set_of_hyp_map tl1_inv_reads loop_start_res.linear in
      (* let tl1_inv_writes = resource_set_of_hyp_map tl1_inv_writes ctx_res.linear in *)
      let tl1_inv = resource_set_of_hyp_map tl1_inv_usage loop_start_res.linear in
      let (_, tl2_inv_writes, _) = Resource_computation.subtract_linear_resource_set ~split_frac:false linear_invariant tl1_inv in (* = I'' *)

      let split_res = if Mlist.is_empty tl2 then Resources.after_trm t_seq else Resources.before_trm (Mlist.nth tl2 0) in (* = R *)
      let (_, split_res_comm, _) = (* R' *)
        Resource_computation.subtract_linear_resource_set ~split_frac:false split_res.linear (linear_invariant @ Resource_contract.parallel_reads_inside_loop l_range contract.parallel_reads)
      in

      (* Remove resources that refer to local variables in tl1 *)
      (* LATER: Run scope destructors and generalize the rest with pure variables *)
      let bound_in_tl1 = Mlist.fold_left (fun acc ti -> (* TODO: gather bound_vars_in_trms *)
          match trm_let_inv ti with
          | Some (vk, v, typ, init) -> Var_set.add v acc
          | None -> acc
        ) Var_set.empty tl1
      in
      let split_res_comm = List.filter (fun (h, formula) ->
          Var_set.disjoint (trm_free_vars formula) bound_in_tl1
        ) split_res_comm
      in

      (* Remove resources that are only leftover parts of a frame by instantiating efracs. *)
      (* Currently use a simple algorithm that considers that each efrac is used at most in one framed context. *)
      let efrac_map =
        ref (List.fold_left (fun efrac_map (efrac, _) -> Var_map.add efrac None efrac_map) Var_map.empty split_res.efracs)
      in
      let try_nullify_frac (frac: formula): bool =
        let open Xoption.OptionMonad in
        let rec aux frac nb_efracs =
          Pattern.pattern_match frac [
            Pattern.(trm_sub !__ (trm_var !__)) (fun base_frac removed_var ->
              Pattern.when_ (Var_map.find_opt removed_var !efrac_map = Some None);
              let* efrac_val = aux base_frac (nb_efracs + 1) in
              efrac_map := Var_map.add removed_var (Some efrac_val) !efrac_map;
              Some efrac_val
            );
            Pattern.(!__) (fun _ ->
              if nb_efracs = 0 then None
              else if nb_efracs = 1 then Some frac
              else Some (trm_div frac (trm_int nb_efracs))
            )
          ]
        in
        Option.is_some (aux frac 0)
      in
      let split_res_comm = List.filter (fun (h, formula) ->
        match Var_map.find_opt h tl1_usage with
        | Some SplittedFrac ->
          begin match formula_read_only_inv formula with
          | Some { frac } when try_nullify_frac frac -> false
          | _ -> true
          end
        | _ -> true
      ) split_res_comm in
      (* LATER: Allow efracs that are not eliminated *)
      let efrac_map = Var_map.mapi (fun efrac efrac_val ->
        match efrac_val with
        | Some efrac_val -> efrac_val
        | None -> failwith (sprintf "At the splitting point, existential fraction %s was not eliminated" efrac.name))
        !efrac_map
      in
      let split_res_without_efracs = List.map (fun (h, formula) -> (h, trm_subst efrac_map formula)) split_res_comm in

      let tl2_usage = Resources.compute_usage_of_instrs tl2 in
      let post_inst_usage = Resource_computation.used_set_to_usage_map (Resources.post_inst t_seq) in
      let usage_after_tl1 = Resource_computation.update_usage_map ~current_usage:tl2_usage ~extra_usage:post_inst_usage in
      let tl1_ensured = List.filter (fun (x, f) ->
        Var_map.find_opt x tl1_usage = Some Ensured &&
          Var_map.mem x usage_after_tl1 &&
          Var_set.disjoint (trm_free_vars f) bound_in_tl1
        ) split_res.pure
      in
      let middle_iter_contract = Resource_set.make ~pure:tl1_ensured ~linear:split_res_without_efracs () in

      let fst_contract = {
        loop_ghosts = contract.loop_ghosts;
        invariant = { contract.invariant with linear = tl1_inv };
        parallel_reads = contract.parallel_reads;
        iter_contract = {
          pre = contract.iter_contract.pre;
          post = middle_iter_contract;
        };
        strict = true;
      } in
      let snd_contract = {
        loop_ghosts = (*List.map (fun (efrac, _) -> (efrac, trm_frac)) split_res.efracs @*) contract.loop_ghosts;
        invariant = { contract.invariant with linear = tl2_inv_writes };
        parallel_reads = tl1_inv_reads @ contract.parallel_reads;
        iter_contract = {
          pre = { middle_iter_contract with pure = tl1_ensured @ contract.iter_contract.pre.pure };
          post = contract.iter_contract.post; (* LATER: Can be slightly more clever here, by removing ensures that are already done by the first loop. *)
        };
        strict = true;
      } in
      fst_contract, snd_contract
  in

  let ta = trm_add_mark mark_loops (trm_for_instrs ~contract:fst_contract l_range tl1) in
  let tb = trm_add_mark mark_loops (trm_copy (trm_for_instrs ~contract:snd_contract l_range tl2)) in
  (ta, tb)
    (* Note: the trm_copy is needed because the loop index in the
       two loops must have a different id. We copy the second loop
       because fission_all_instr process them from the end. *)

(* [fission_on]: split loop [t] into two loops

    [index]: index of the splitting point
    [t]: ast of the loop
    *)
let fission_on (mark_loops : mark) (mark_between_loops : mark) (index : int) (t : trm) : trm =
  let (ta,tb) = fission_on_as_pair mark_loops index t in
  trm_seq_helper ~braces:false [ Trm ta; Mark mark_between_loops; Trm tb ]

(* [fission tg]: expects the target [tg] to point somewhere inside the body of the simple loop
   It splits the loop in two loops, the spliting point is trm matched by the relative target.

   @correctness: Reads in new second loop need to never depend on writes on
   first loop after index i. Writes in new second loop need to never overwrite
   writes in first loop after index i. *)
let%transfo fission_basic ?(mark_loops : mark = no_mark) ?(mark_between_loops : mark = no_mark) (tg : target) : unit =
  (* TODO: figure out best nobrace/iter/resource interleaving *)
  Nobrace_transfo.remove_after (fun _ ->
    Target.iter (fun p_before ->
      let (p_seq, split_i) = Path.extract_last_dir_before p_before in
      let p_loop = Path.parent_with_dir p_seq Dir_body in
      (* DEBUG: let debug_p = Path.parent p_loop in
      Show.res ~msg:"res1" ~ast:(get_trm_at_exn (target_of_path debug_p))
      ); *)
      Resources.required_for_check ();
      apply_at_path (fission_on mark_loops mark_between_loops split_i) p_loop;
    ) tg
  );
  Resources.justif_correct "loop resources where successfully split"

(* TODO: valid in C but not C++? *)
let normalize_loop_step (s : loop_step) : loop_step =
  match s with
  | Pre_inc -> Post_inc
  | Post_inc -> Post_inc
  | Pre_dec -> Post_dec
  | Post_dec -> Post_dec
  | Step amount ->
    if is_trm_int 1 amount then Post_inc else s

let same_loop_step (a : loop_step) (b : loop_step) : bool =
  match ((normalize_loop_step a), (normalize_loop_step b)) with
  (* | (Pre_inc, Pre_inc) -> true *)
  | (Post_inc, Post_inc) -> true
  (* | (Pre_dec, Pre_dec) -> true *)
  | (Post_dec, Post_dec) -> true
  | (Step s_a, Step s_b) -> Internal.same_trm s_a s_b
  | _ -> false

let same_loop_range
  (range1 : loop_range)
  (range2 : loop_range) : bool =
  Internal.same_trm range1.start range2.start &&
  (range1.direction = range2.direction) &&
  Internal.same_trm range1.stop range2.stop &&
  same_loop_step range1.step range2.step

let same_loop_index (a : loop_range) (b : loop_range) : bool =
  assert (a.index.qualifier = [] && b.index.qualifier = []);
  a.index.name = b.index.name

(* [t] is a sequence;
   [index] is the index of the first loop to fuse in seq [t].
   Its annotations are kept.
   if [upwards], [index + 1] is the index of the second loop to fuse.
   if [not upwards], [index - 1] is the index of the second loop to fuse.
  *)
let fusion_on (index : int) (upwards : bool) (t : trm) : trm =
  let instrs = trm_inv
    ~error:"Loop_basic.fusion_on: expected sequence"
    trm_seq_inv t in
  (* LATER:
     (above: trm_seq_update_multi
       ~error_if_not_seq:...)
     Mlist.update_multi index nb (fun mlist -> mlist') +
     let ((m1, l1, m2), (m3, l2, m4)) = List.inv2 Mlist.inv +
     Mlist.make [(m1, l, m4)]
     *)
  let (update_index, target_loop_i) =
    if upwards then (index, 0) else (index - 1, 1)
  in
  let (other_instrs, loops_ml) = Mlist.extract update_index 2 instrs in
  let loops = Mlist.to_list loops_ml in
  let lt = List.nth loops target_loop_i in
  let loops_ri = List.map (
    trm_inv
    ~error:"Loop_basic.fusion_on: expected simple loop"
    trm_for_inv_instrs
  ) loops in
  match loops_ri with
  | [(loop_range1, loop_instrs1, contract1); (loop_range2, loop_instrs2, contract2)] ->
    (* DEPRECATED: need to rename index anyway since #var-id
    if not (same_loop_index loop_range1 loop_range2) then
      trm_fail t "Loop_basic.fusion_on: expected matching loop indices"; *)
    if not (same_loop_range loop_range1 loop_range2) then
      trm_fail t "Loop_basic.fusion_on: expected matching loop ranges";
    let new_loop_range, _, _ = List.nth loops_ri target_loop_i in
    let idx1 = loop_range1.index in
    let idx2 = loop_range2.index in

    let contract = if contract1.strict && contract2.strict then
      let open Resource_formula in

      let loop1 = List.nth loops 0 in
      let loop2 = List.nth loops 1 in
      if Var_set.mem loop_range1.index (Resource_set.used_vars contract1.invariant) then
        trm_fail loop1 "loop invariant uses loop index";
      if Var_set.mem loop_range2.index (Resource_set.used_vars contract2.invariant) then
        trm_fail loop2 "loop invariant uses loop index";

      let usage1 = Resources.usage_of_trm loop1 in
      let usage2 = Resources.usage_of_trm loop2 in
      let ctx2 = Resources.before_trm loop2 in

      (* TODO: restrict to relevant invariant resources *)

      (* FIXME: Resources.assert_usages_commute API was not flexible enough *)
      let interference = Resources.collect_interferences usage1 usage2 in
      (* TODO: factorize, also used in fission *)
      let resource_set_of_hyp_map (hyps: 'a Hyp_map.t) (resources: resource_item list): resource_item list =
        List.filter (fun (h, _) -> Hyp_map.mem h hyps) resources
      in
      let interference_resources = resource_set_of_hyp_map interference ctx2.linear in
      let shared1 = contract1.invariant.linear @ contract1.parallel_reads in
      let shared2 = contract2.invariant.linear @ contract2.parallel_reads in
      let resources_in_common ra rb =
        let (used, _, _, _) = Resource_computation.partial_extract_linear_resource_set interference_resources shared1 in
        used <> []
      in
      (* TODO: instead of filtering, add featrues to collect interference to match paper formula *)
      if resources_in_common interference_resources shared1 ||
         resources_in_common interference_resources shared2
      then trm_fail t (Resources.string_of_interference interference);

      let (pre1, post1, pre2, post2) =
        if upwards
        then (
          contract1.iter_contract.pre,
          contract1.iter_contract.post,
          Resource_set.subst_var idx2 (trm_var idx1) contract2.iter_contract.pre,
          Resource_set.subst_var idx2 (trm_var idx1) contract2.iter_contract.post)
        else (
          Resource_set.subst_var idx1 (trm_var idx2) contract1.iter_contract.pre,
          Resource_set.subst_var idx1 (trm_var idx2) contract1.iter_contract.post,
          contract2.iter_contract.pre,
          contract2.iter_contract.post)
      in
      let (_, post1', pre2', _) =
        (* TODO: the same on resource_set to match paper *)
        Resource_computation.partial_extract_linear_resource_set post1.linear pre2.linear in
      {
        loop_ghosts = contract1.loop_ghosts @ contract2.loop_ghosts;
        invariant = Resource_set.union contract1.invariant contract2.invariant;
        parallel_reads = contract1.parallel_reads @ contract2.parallel_reads;
        iter_contract = {
          pre = Resource_set.union pre1 { pre2 with linear = pre2' };
          post = Resource_set.union post2 { post1 with linear = post1' };
        };
        strict = true;
      }
    else if !Flags.check_validity then
      trm_fail t "requires annotated for loops to check validity"
    else
      empty_loop_contract
    in

    let loop_instrs1', loop_instrs2' =
      if upwards
      then loop_instrs1, Mlist.map (trm_subst_var idx2 (trm_var idx1)) loop_instrs2
      else Mlist.map (trm_subst_var idx1 (trm_var idx2)) loop_instrs1, loop_instrs2
    in
    let new_loop_instrs = Mlist.merge loop_instrs1' loop_instrs2' in
    (* TODO: trm_for_update on loop1? *)
    let new_loop = trm_for_instrs ~annot:lt.annot ?loc:lt.loc ~contract new_loop_range new_loop_instrs in
    let new_instrs = Mlist.insert_at update_index new_loop other_instrs in
    trm_seq ~annot:t.annot ?loc:t.loc new_instrs
  | _ -> failwith "unreachable"

(* [fusion]: expects the target [tg] to point at a loop that is followed by another loop with the same range (start, stop, step).
  Merges the two loops into a single one, sequencing the loop bodies into a new loop body:

  for (int i = start; i < stop; i += step) {
    body1
  }
  for (int i = start; i < stop; i += step) {
    body2
  }

  -->

  for (int i = start; i < stop; i += step) {
    body1;
    body2
  }
 *)
let%transfo fusion ?(upwards : bool = true) (tg : target) : unit =
  Target.iter (fun p ->
    let (index, p_seq) = Path.index_in_seq p in
    Resources.required_for_check ();
    Target.apply_at_path (fusion_on index upwards) p_seq;
    Resources.required_for_check ();
  ) tg;
  Resources.justif_correct "loop resources where successfully merged"

(* [grid_enumerate index_and_bounds tg]: expects the target [tg] to point at a loop iterating over
    a grid. The grid can be of any dimension.
    Loop  [tg] then is transformed into nested loops
    where the number of nested loops is equal to the number of dimensions.
      [index_and_bounds] - is a list of pairs, where each pair denotes the index and the bound
        of the loop iterating over a specific dimension.
    Ex: Assume A = X * Y * Z, and [index_and_bounds] = [("x","X");("y","y");("z","Z")] and the result is

      for (int a = 0; a < A; a++){        for (int x = 0; x < X; x++){
        .......                       =>    for (int y = 0; y < Y; y++){
      }                                       for (int z = 0; z < Z, z++){
                                                int a = ((x * Y) + y)*Z + z
                                                ...
                                              }
                                            }
                                          } *)
let%transfo grid_enumerate (index_and_bounds : (string * trm) list) (tg : target) : unit =
  apply_on_targets (Loop_core.grid_enumerate index_and_bounds) tg

(* [unroll ~braces ~my_mark tg]: expects the target to point at a simple loop of the shape
    for (int i = a; i < a + C; i++) or for (int i = 0; i < C; i++)
      then it will move the instructions out of the loop by replacing
      the index i occurrence with a + j in and j in the second case where
      j is an integer in range from 0 to C.

    Assumption: Both a and C should be declared as constant variables. *)
let%transfo unroll ?(inner_braces : bool = false) ?(outer_seq_with_mark : mark  = no_mark) ?(subst_mark : mark = no_mark) (tg : target): unit =
  Trace.justif_always_correct ();
  Nobrace_transfo.remove_after (fun _ ->
    apply_on_targets (Loop_core.unroll inner_braces outer_seq_with_mark subst_mark) tg)


type empty_range_mode =
| Generate_if
| Arithmetically_impossible
| Produced_resources_uninit_after

(* [move_out_on trm_index t]: moves an invariant instruction just before loop [t],
    [trm_index] - index of that instruction on its surrouding sequence (just checks that it is 0),
    [t] - ast of the for loop.
  *)
let move_out_on (instr_mark : mark) (loop_mark : mark) (empty_range: empty_range_mode) (trm_index : int) (t : trm) : trm =
  if (trm_index <> 0) then failwith "Loop_basic.move_out: not targeting the first instruction in a loop";
  let error = "Loop_basic.move_out: expected for loop" in
  let (range, body, contract) = trm_inv ~error trm_for_inv t in
  let instrs = trm_inv ~error trm_seq_inv body in
  let instr = Mlist.nth instrs 0 in
  let rest = Mlist.pop_front instrs in

  if !Flags.check_validity then begin
    if Var_set.mem range.index (trm_free_vars instr) then
      (* NOTE: would be checked by var ids anyway *)
      trm_fail instr "Loop_basic.move_out: instruction uses loop index";
    Resources.assert_dup_instr_redundant 0 (Mlist.length instrs - 1) body;

    begin match empty_range with
    | Generate_if -> ()
    | Arithmetically_impossible -> failwith "Arithmetically_impossible is not implemented yet"
    | Produced_resources_uninit_after ->
      if not contract.strict then failwith "Need the for loop contract to be strict";
      let instr_usage = Resources.usage_of_trm instr in
      let invariant_written_by_instr = List.filter (Resources.(linear_usage_filter instr_usage keep_written)) contract.invariant.linear in
      List.iter (fun (_, f) -> match Resource_formula.formula_uninit_inv f with
        | Some _ -> ()
        | None -> trm_fail instr "The instruction cannot be moved out because it consumes resources that are not uninitialized after the loop (and the loop range could be empty)"
      ) invariant_written_by_instr
    end;

    Trace.justif "instructions from following iterations are redundant with first iteration"
  end;

  let generate_if = (empty_range = Generate_if) in
  let contract =
    if generate_if || not contract.strict then
      contract
    else
      (* FIXME: this still requires resources to update contract even when not checking validity! *)
      let resources_after = Xoption.unsome ~error:"Loop_basic.move_out: requires computed resources" instr.ctx.ctx_resources_after in
      let _, new_invariant, _ = Resource_computation.subtract_linear_resource_set resources_after.linear (Resource_contract.parallel_reads_inside_loop range contract.parallel_reads @ contract.iter_contract.pre.linear) in
      { contract with invariant = { contract.invariant with linear = new_invariant } }
  in

  let loop = trm_for ~contract range (trm_seq rest) in
  let non_empty_cond = trm_ineq range.direction range.start range.stop in
  let instr_outside = if generate_if then trm_if non_empty_cond instr (trm_unit ()) else instr in
  trm_seq_nobrace_nomarks [
    trm_add_mark instr_mark instr_outside;
    trm_add_mark loop_mark loop]

(** [move_out tg]: expects the target [tg] to point at the first instruction inside the loop
    that is not dependent on the index of the loop or any local variable.
    Then it will move it outside the loop.

    {@c[
    for (i) {
      tg;
      middle-instrs
    }
    will become
    tg; // or: if (range not empty) tg;
    for (i) {
      middle-instrs
    }
    ]}

    Correctness check:
    (1) tg is idempotent/not self-interfering:
      tg uses resources RO(A) and Uninit(B), it must not use any full permission,
      it must not have any produce apart from the RW coming from B
    (2) Duplicating tg after middle-instrs would be redundant:
      in middle-instrs usage, resources A and B can only be used in RO mode.
      This is equivalent to say that there is no usage of A or B in Uninit or RW mode in middle-instr.
    (3) If the loop range can be empty, adding the extra tg instruction must not change the behaviour.
      Three methods to handle that:
      - Add an if on tg outside the loop
      - Prove that the loop range is never empty
      - All resources in B are uninit in the loop contract
*)
let%transfo move_out ?(instr_mark : mark = no_mark) ?(loop_mark : mark = no_mark) ?(empty_range: empty_range_mode = Produced_resources_uninit_after) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    Target.iter (fun p ->
      Resources.required_for_check ();
      let i, p = Path.index_in_surrounding_loop p in
      apply_at_path (move_out_on instr_mark loop_mark empty_range i) p
  ) tg)

let move_out_alloc_on (empty_range: empty_range_mode) (trm_index : int) (t : trm) : trm =
  if (trm_index <> 0) then failwith "not targeting the first instruction in a loop";
  let error = "expected for loop" in
  let (range, body, contract) = trm_inv ~error trm_for_inv t in
  let instrs = trm_inv ~error trm_seq_inv body in
  let instr_count = Mlist.length instrs in
  if (instr_count < 2) then failwith "expected at least two instructions";
  let alloc_instr = Mlist.nth instrs 0 in
  let free_instr = Mlist.nth instrs (instr_count - 1) in
  let (_, rest) = Mlist.extract 1 (instr_count - 2) instrs in

  if !Flags.check_validity then begin
    (* NOTE: would be checked by var ids anyway *)
    if Var_set.mem range.index (trm_free_vars alloc_instr) then
      trm_fail alloc_instr "allocation instruction uses loop index";
    if Var_set.mem range.index (trm_free_vars free_instr) then
      trm_fail free_instr "free instruction uses loop index";
    (* Resources.assert_dup_instr_redundant 0 (Mlist.length instrs - 1) body;
      --> We know that `free x; alloc x = ()`
      *)

    let error = "expected MALLOCN instr" in
    let (_, _, _, alloc_init) = trm_inv ~error trm_let_inv alloc_instr in
    let _ = trm_inv ~error Matrix_core.alloc_inv_with_ty alloc_init in
    let error = "expected MFREEN instr" in
    let _ = trm_inv ~error Matrix_trm.free_inv free_instr in

    (* FIXME: Code duplicated from move_out_on *)
    begin match empty_range with
    | Generate_if -> ()
    | Arithmetically_impossible -> failwith "Arithmetically_impossible is not implemented yet"
    | Produced_resources_uninit_after ->
      if not contract.strict then failwith "Need the for loop contract to be strict";
      let assert_instr_uses_no_invariant instr =
        let instr_usage = Resources.usage_of_trm instr in
        let invariant_usage = List.filter (Resources.(linear_usage_filter instr_usage keep_touched_linear)) contract.invariant.linear in
        if invariant_usage <> [] then
          trm_fail instr "does not support moving out instructions that touch invariant resources"
      in
      assert_instr_uses_no_invariant alloc_instr;
      assert_instr_uses_no_invariant free_instr
    end;

    Trace.justif "instructions from following iterations are redundant with first iteration"
  end;

  let generate_if = (empty_range = Generate_if) in
  let contract =
    if generate_if || not contract.strict then
      contract
    else
      (* FIXME: this still requires resources to update contract even when not checking validity! *)
      let resources_after = Xoption.unsome ~error:"requires computed resources" alloc_instr.ctx.ctx_resources_after in
      let _, new_invariant, _ = Resource_computation.subtract_linear_resource_set resources_after.linear (Resource_contract.parallel_reads_inside_loop range contract.parallel_reads @ contract.iter_contract.pre.linear) in
      { contract with invariant = { contract.invariant with linear = new_invariant } }
  in

  let loop = trm_for ~annot:t.annot ~contract range (trm_seq rest) in
  let wrap_instr instr =
    let non_empty_cond = trm_ineq range.direction range.start range.stop in
    if generate_if then trm_if non_empty_cond instr (trm_unit ()) else instr
  in
  trm_seq_nobrace_nomarks [
    wrap_instr alloc_instr;
    loop;
    wrap_instr free_instr]

(** [move_out_alloc ~empty_range tg]: same as [move_out], but supports moving out an allocation
    instruction together with its corresponding deallocation (that must be at the end of the loop).

    TODO: generalize [move_out] and [move_out_alloc] to moving out the any first/last group of instrs (I1 and I2) at once, if (I2; I1) is a no-op.
  *)
let%transfo move_out_alloc ?(empty_range: empty_range_mode = Produced_resources_uninit_after) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    Target.iter (fun p ->
      Resources.required_for_check ();
      let i, p = Path.index_in_surrounding_loop p in
      apply_at_path (move_out_alloc_on empty_range i) p
  ) tg)

(* [unswitch tg]:  expects the target [tg] to point at an if statement with a constant condition
     (not dependent on loop index or local variables) inside a loop.  Then it will take that
      if statment outside the loop.

   @correctness: requires that the loop is parallelizable *)
let%transfo unswitch (tg : target) : unit =
  Nobrace_transfo.remove_after ( fun _ ->
  apply_on_transformed_targets(Path.index_in_surrounding_loop)
    (fun t (i, p) -> Loop_core.unswitch i t p) tg)


(* [to_unit_steps index tg]: expects target [tg] to point at a for loop
    [index] - denotes the new index for the transformed loop
        by default is an empty string. The reason for that is to check if the user
        gave the name of the new index of not. If not then [index] = unit_index
        where index is the index of the targeted loop.

    Assumption:
      The targeted loop should be of the form:
        for (int i = a; i < b; i+=B){ s += i },
        and it assumes that B divides (b-a). It then
        transforms the targeted loop into the following form:
          for (int index = 0; index < ...; index++) {
            int i = (a + (j * B));
            s += i;
           } *)
let%transfo to_unit_steps ?(index : string = "" ) (tg : target) : unit =
  apply_on_targets (Loop_core.to_unit_steps index) tg

(* [fold ~direction index start stop step tg]: expects the target [tg] to point at the first instruction in a sequence
    and it assumes that the sequence containing the target [tg] is composed of a list of instructions which
    can be expressed into a single for loop with [index] [direction] [start] [nb_instructions] and [step] as loop
    components. *)
let%transfo fold ~(index : string) ~(start : int) ~(step : int) (tg : target) : unit =
  apply_on_targets (
    Loop_core.fold index start step
  ) tg

(* [split_range nb cut tg]: expects the target [tg] to point at a simple loop
    then based on the arguments nb or cut it will split the loop into two loops. *)
let%transfo split_range ?(nb : int = 0) ?(cut : trm = trm_unit()) (tg : target) : unit =
  Nobrace_transfo.remove_after( fun _ ->
    apply_on_targets (Loop_core.split_range nb cut) tg )

type shift_kind =
| ShiftBy of trm
| StartAtZero
| StartAt of trm
| StopAt of trm

let shift_kind_to_string = function
| ShiftBy t -> "ShiftBy " ^ (AstC_to_c.ast_to_string t)
| StartAtZero -> "StartAtZero"
| StartAt t -> "StartAt " ^ (AstC_to_c.ast_to_string t)
| StopAt t -> "StopAt " ^ (AstC_to_c.ast_to_string t)

let ghost_group_shift = toplevel_var "group_shift"
let ghost_group_unshift = toplevel_var "group_unshift"

(* [shift_on index kind]: shifts a loop index to start from zero or by a given amount. *)
let shift_on (index : string) (kind : shift_kind) (t : trm): trm =
  let index' = new_var index in
  let error = "Loop_basic.shift_on: expected a target to a simple for loop" in
  let ({ index; start; direction; stop; step }, body_terms, contract) =
    trm_inv ~error trm_for_inv_instrs t in
  if !Flags.check_validity then begin
    match kind with
    | ShiftBy v | StartAt v | StopAt v ->
      if Resources.trm_is_pure v then
        (* TODO: also works for read-only *)
        Trace.justif "shifting by a pure expression is always correct"
      else
        trm_fail v "shifting by a non-pure expression is not yet supported, requires checking that expression is read-only, introduce a binding with 'Sequence.insert' to workaround" (* TODO: combi doing this *)
    | StartAtZero -> Trace.justif "shifting to zero is always correct, loop range is read-only"
  end;
  (* spec:
    let start' = trm_add start shift in
    let stop' = trm_add stop shift in *)
  let (shift, start', stop') = match kind with
  (* spec:
    let start' = trm_add start shift in
    let stop' = trm_add stop shift in *)
  | ShiftBy s -> (s, trm_add start s, trm_add stop s)
  (* NOTE: assuming int type *)
  | StartAtZero -> (trm_minus start, trm_int 0, trm_sub stop start)
  | StartAt v -> (trm_sub v start, v, trm_add stop (trm_sub v start))
  | StopAt v -> (trm_sub v stop, trm_add start (trm_sub v stop), v)
  in
  let index_expr = trm_sub (trm_var index') shift in
  (* NOTE: assuming int type if no type is available *)
  let body_terms' = Mlist.push_front (
    trm_let_immut (index, (Option.value ~default:(typ_int ()) start.typ)) index_expr) body_terms in
  let range' = { index = index'; start = start'; direction; stop = stop'; step } in
  begin if not contract.strict then
    trm_for_instrs ~annot:t.annot range' body_terms'
    (* trm_fail t "expected loop contract when checking validity" ? *)
  else
    let open Resource_formula in
    let shift_ghosts ghost =
      List.map (fun (_, formula) ->
        let i = new_var index.name in
        let items = formula_fun [i, typ_int ()] None (trm_subst_var index (trm_var i) formula) in
        Resource_trm.ghost (ghost_call ghost [
          "start", start; "stop", stop; "step", loop_step_to_trm step; "items", items;
          "shift", shift; "new_start", start'; "new_stop", stop'])
      )
    in
    let ghosts_before = shift_ghosts ghost_group_shift contract.iter_contract.pre.linear in
    let ghosts_after = shift_ghosts ghost_group_unshift contract.iter_contract.post.linear in
    let contract' = Resource_contract.loop_contract_subst (Var_map.singleton index index_expr) contract in
    trm_seq_nobrace_nomarks (ghosts_before @ [
     trm_for_instrs ~annot:t.annot ~contract:contract' range' body_terms'
     ] @ ghosts_after)
  end

(** [shift index kind]: shifts a loop index range according to [kind], using a new [index] name.

  validity: expressions used in shifting must be referentially transparent, other expressions can be bound before using [Sequence.insert].
  *)
let%transfo shift ?(reparse : bool = false) (index : string) (kind : shift_kind) (tg : target) : unit =
  (* FIXME: having to think about reparse here is not great *)
  reparse_after ~reparse (fun tg ->
    Nobrace_transfo.remove_after (fun () ->
      Target.apply_at_target_paths (shift_on index kind) tg)
  ) tg

type extension_kind =
| ExtendNothing
| ExtendToZero
| ExtendTo of trm
| ExtendBy of trm

let extension_kind_to_string = function
| ExtendNothing -> "ExtendNothing"
| ExtendToZero -> "ExtendToZero"
| ExtendTo t -> "ExtendTo " ^ (AstC_to_c.ast_to_string t)
| ExtendBy t -> "ExtendBy " ^ (AstC_to_c.ast_to_string t)

let extend_range_on (start_extension : extension_kind) (stop_extension : extension_kind) (t : trm) : trm =
  let error = "Loop_basic.extend_range_on: expected a target to a simple for loop" in
  let ({ index; start; direction; stop; step }, body, _contract) = trm_inv ~error trm_for_inv t in
  assert (direction = DirUp);
  (* TODO: does it work in other cases?
     assert (is_step_one step); *)
  (* avoid merging new ifs with previous ones *)
  let added_if = ref false in
  let make_if cond body =
    let should_merge = !added_if in
    added_if := true;
    let t = trm_if cond body (trm_unit ()) in
    if should_merge
    then Option.get (If_core.may_merge t)
    else t
  in
  let if_before_stop body = trm_seq_nomarks [make_if (trm_lt (trm_var index) stop) body] in
  let if_after_start body = trm_seq_nomarks [make_if (trm_le start (trm_var index)) body] in
  let (stop', body') = begin match stop_extension with
  | ExtendNothing -> (stop, body)
  | ExtendToZero -> failwith "not implemented yet"
  | ExtendTo v -> (v, if_before_stop body)
  | ExtendBy v -> (trm_add stop v, if_before_stop body)
  end in
  let (start', body'') = begin match start_extension with
  | ExtendNothing -> (start, body')
  | ExtendToZero -> (trm_int 0, if_after_start body')
  | ExtendTo v -> (v, if_after_start body')
  | ExtendBy v -> (trm_sub start v, if_after_start body')
  end in
  trm_for ~annot:t.annot { index; start = start'; direction; stop = stop'; step } body''

(* [extend_range]: extends the range of a loop on [lower] and/or [upper] bounds.
   The body of the loop is guarded by ifs statements, doing nothing on the extension points.

   For this to be correct, the loop bounds must be extended, not shrinked.
  *)
let%transfo extend_range ?(start : extension_kind = ExtendNothing) ?(stop : extension_kind = ExtendNothing) (tg : target) : unit =
  Target.apply_at_target_paths (extend_range_on start stop) tg

(* [rename_index new_index]: renames the loop index variable *)
let%transfo rename_index (new_index : string) (tg : target) : unit =
  apply_on_targets (Loop_core.rename_index new_index) tg;
  Trace.justif "renaming loop index is always correct (unique ids avoid name conflicts)"

(* FIXME: duplicated code from tiling. *)
let slide_on (tile_index : string) (bound : tile_bound) (tile_size : trm) (tile_step : trm) (t : trm) : trm =
  let error = "Loop_basic.slide_on: only simple loops are supported." in
  let ({ index; start; direction; stop; step }, body, _contract) = trm_inv ~error trm_for_inv t in
  let tile_index = new_var (Tools.string_subst "${id}" index.name tile_index) in
  let tile_bound =
   if is_step_one step then trm_add (trm_var tile_index) tile_size else trm_add (trm_var tile_index) (trm_mul tile_size (loop_step_to_trm step)) in
  let inner_loop =
  begin match bound with
  | TileBoundMin ->
    let tile_bound =
    trm_apps (trm_var (name_to_var "min")) [stop; tile_bound] in
    trm_for { index; start = trm_var tile_index; direction; stop = tile_bound; step } body
  | TileDivides ->
    trm_for { index; start = trm_var tile_index; direction; stop = tile_bound; step } body
  | TileBoundAnd ->
    let init = trm_let_mut (index, typ_int ()) (trm_var tile_index) in
    let cond = trm_and (trm_ineq direction (trm_var_get index)
      (if is_step_one step
        then (trm_add (trm_var tile_index) tile_size)
        else (trm_add (trm_var tile_index) (trm_mul tile_size (loop_step_to_trm step) ) ))) (trm_ineq direction (trm_var_get index) stop)
      in
    let step =  if is_step_one step then trm_apps (trm_unop Unop_post_inc) [trm_var index]
      else trm_prim_compound Binop_add (trm_var index) (loop_step_to_trm step) in
    let new_body = trm_subst_var index (trm_var_get index) body in
    trm_for_c init cond step new_body
  end in
  (* NOTE: only outer loop differs from tiling? *)
  let may_scale x = if is_step_one step
    then x else trm_mul x (loop_step_to_trm step) in
  let outer_loop_step = Step (may_scale tile_step) in
  let outer_stop = (trm_add stop (may_scale (trm_sub tile_step tile_size))) in
  let outer_loop =
      trm_for { index = tile_index; start; direction; stop = outer_stop; step = outer_loop_step } (trm_seq_nomarks [inner_loop])
  in
  trm_pass_labels t outer_loop

(* [slide]: like [tile] but with the addition of a [step] parameter that controls how many iterations stand between the start of two tiles. Depending on [step] and [size], some iterations may be discarded or duplicated.
*)
let%transfo slide ?(index : string = "b${id}")
  ?(bound : tile_bound = TileBoundMin)
  ~(size : trm)
  ~(step : trm)
  (tg : target) : unit =
  Target.apply_at_target_paths (slide_on index bound size step) tg

let delete_void_on (i : int) (t_seq : trm) : trm option =
  (* 1. check empty body *)
  Option.bind (trm_seq_nth_inv i t_seq) (fun t_loop ->
    Option.bind (trm_for_inv_instrs t_loop) (fun (range, body, _) ->
      if Mlist.is_empty body && Resources.trm_is_pure range.start && Resources.trm_is_pure range.stop && Resources.trm_is_pure (loop_step_to_trm range.step)
      (* TODO: No need to check pure range if this is a global invariant of Trm_for *)
      (* 2. delete *)
      then Some (Sequence_core.delete_at i t_seq)
      else None
    ))

(* [delete_void]: deletes a loop with empty body. *)
let%transfo delete_void (tg : target) : unit =
  Trace.justif_always_correct ();
  Target.iter (fun p ->
    let (i, p_seq) = Path.index_in_seq p in
    apply_at_path (fun t_seq ->
      match delete_void_on i t_seq with
      | Some t2 -> t2
      | None ->
        let loop_t = Target.resolve_path p in
        trm_fail loop_t "Loop_basic.delete_void: expected a simple loop with empty body, surrounded by a sequence"
    ) p_seq
  ) tg
