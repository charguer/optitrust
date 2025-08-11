open Prelude
open Target
open Matrix_trm
open Loop_core

(** [color nb_colors i_color tg]: expects the target [tg] to point at a simple for  loop,
   let's say [for (int i = start; i < stop; i += step) { body } ].
   [nb_colors] - an expression denoting the number of colors (e.g., ["2"]),
   [index] - denotes a fresh name to use as index for iterating over colors.

   In case [step = 1]:
   {@c[for (int index = 0; index < nb_color; index++) {
      for (int i = index; i < stop; i += nb_color) { body }]}.

   In the general case, it produces:
   {@c[for (int index = 0; index < nb_color; index++) {
      for (int i = index*step; i < stop; i += step*nb_color) { body }]}. *)
let%transfo color (nb_colors : trm) ?(index : string option) (tg : target) : unit =
  apply_at_target_paths (Loop_core.color_on nb_colors index) tg

(** [tile tile_size index tg]: expects the target [tg] to point at a simple loop,
   say [for (int i = start; i < stop; i += step) { body } ].
   divides - denotes a flag to know if tile_size divides the size of the array or not
   [tile_size] - denotes the width of the tile (e.g., ["2"])
   [index] - denotes a fresh name to use as index for iterating over tiles.
   [bound] - can be one of
      - TileBoundMin: generates a constraint of the form  [i < min(X, bx+B)]
      - TileBoundAnd: generates a constraint of the form [i <  X && i < bx+B]
      - TileDivides: generates a constraint of the form [i < X], which is only true if B divides X

   It produces:
   {@c[for (int index = 0; index < stop; index += tile_size) {
      for (int i = index; i < min(X, bx+B); i++) { body }]}. *)
let%transfo tile ?(index : string = "b${id}")
         ?(bound : tile_bound = TileBoundMin)
         (tile_size : trm) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
    Target.apply_at_target_paths (Loop_core.tile_on index bound tile_size) tg
  )

(** <private> *)
let collapse_analyse (ri_rj_body : (loop_range * loop_contract * loop_range * loop_contract * trm) option ref) (t : trm) : trm =
  let error = "expected 2 nested simple loops" in
  let (ranges, body) = trm_inv ~error (trm_fors_inv 2) t in
  let (ri, ci) = List.nth ranges 0 in
  let (rj, cj) = List.nth ranges 1 in
  if not (is_trm_int 0 ri.start) || not (is_trm_int 0 rj.start) then
    trm_fail t "non-zero range starts are not yet supported: use loop shift first";
  if not (trm_is_one (ri.step)) || not (trm_is_one (rj.step)) then
    trm_fail t "non-unary range steps are not yet supported: use loop scale first";
  if ri.direction <> DirUp || rj.direction <> DirUp then
    trm_fail t "non-increasing range directions are not yet supported: use loop reverse first";
  ri_rj_body := Some (ri, ci, rj, cj, body);
  t

let ghost_group_collapse = toplevel_var "group_collapse"
let ghost_group_uncollapse = toplevel_var "group_uncollapse"
let ghost_ro_group_collapse = toplevel_var "ro_group_collapse"
let ghost_ro_group_uncollapse = toplevel_var "ro_group_uncollapse"

(** <private> *)
let collapse_on (simpl_mark : mark) (index : string)
  (ri : loop_range) (ci : loop_contract) (rj : loop_range) (cj : loop_contract)
  (body : trm) (t : trm) : trm =
  let nbi = ri.stop (* start = 0; trm_sub ri.stop ri.start *) in
  let nbj = rj.stop (* start = 0; trm_sub rj.stop rj.start *) in
  let k = new_var (index |>
    Tools.string_subst "${i}" ri.index.name |>
    Tools.string_subst "${j}" rj.index.name) in
  let var_k = trm_var ~typ:(typ_int) k in
  let rk = {
    index = k; start = trm_int 0; stop = trm_add_mark simpl_mark (trm_mul_int nbi nbj);
    direction = DirUp; step = trm_step_one ()
  } in
  let new_i = trm_add_mark simpl_mark (* (trm_add ri.start = 0 *) (trm_trunc_div_int var_k nbj) in
  let new_j = trm_add_mark simpl_mark (* (trm_add rj.start = 0 *) (trm_trunc_mod_int var_k nbj) in
  let subst = Var_map.(empty |> add ri.index new_i |> add rj.index new_j) in
  (* TODO: works when invariants are the same, and pre/post are the same modulo a star,
    still need to insert ghosts to flatten the double star. *)
  (* group_collapse / group_uncollapse*)
  let open Resource_formula in
  let add_collapse_ghost ghost ghost_ro =
    List.map (fun (_, formula) ->
      (* TODO: factorize generic ghost mode code for all transfos *)
      let ghost, formula = match formula_read_only_inv formula with
      | Some { formula } -> ghost_ro, formula
      | None -> ghost, formula
      in
      let items = trm_copy (formula_fun [ri.index, typ_int; rj.index, typ_int] formula) in
      Resource_trm.ghost (ghost_call ghost [
        "n", nbi; "m", nbj; "items", items
      ])
    )
  in
  let ghosts_before = add_collapse_ghost ghost_group_collapse ghost_ro_group_collapse cj.iter_contract.pre.linear in
  let ghosts_after = add_collapse_ghost ghost_group_uncollapse ghost_ro_group_uncollapse cj.iter_contract.post.linear in
  let contract = Resource_contract.loop_contract_subst subst cj in
  let body2 = if !Flags.check_validity then
    let instrs, _ = trm_inv ~error:"expected seq" trm_seq_inv body in
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
      Trm (assume (formula_geq ~typ:typ_int ri.stop (trm_int 0)));
      Trm (assume (formula_geq ~typ:typ_int rj.stop (trm_int 0)));
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

(** [hoist x_step tg]: expects [tg] to point at a variable declaration inside a
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
(* TODO: clean up code *)
let hoist_on (name : string)
             (mark_alloc : mark) (mark_free : mark) (mark_tmp_var : mark)
             (arith_f : trm -> trm)
             (decl_index : int) (t : trm) : trm =
  let error = "Loop_basic.hoist_on: only simple loops are supported" in
  let (range, body, contract) = trm_inv ~error trm_for_inv t in
  let { index; start; direction; stop; step} = range in
  assert (direction = DirUp); (* TODO: other directions *)
  let (array_size, new_index) =
    if trm_is_one step then
      (trm_sub_int stop start, trm_sub_int (trm_var index) start)
    else
      (* i = start; i < stop; i += step *)
      let trm_ceil_div a b =
        trm_trunc_div_int (trm_add_int a (trm_sub_int b (trm_int 1))) b
      in
      (trm_ceil_div (trm_sub_int stop start) step,
        trm_trunc_div_int (trm_sub_int (trm_var index) start) step)
  in
  let body_instrs, _ = trm_inv ~error trm_seq_inv body in
  let elem_ty = ref typ_auto in
  let old_var = ref dummy_var in
  let new_var = ref dummy_var in
  let new_dims = ref [] in
  let update_decl (decl : trm) : trm =
    let error = "Loop_basic.hoist_on: expected variable declaration with MALLOCN initialization" in
    let (x, etyp, dims) = trm_inv ~error Matrix_core.let_alloc_uninit_inv decl in
    old_var := x;
    new_var := Ast.new_var (Tools.string_subst "${var}" x.name name);
    elem_ty := etyp;
    new_dims := (arith_f array_size) :: dims;
    let partial_indices = (arith_f new_index) ::
      (List.init (List.length dims) (fun _ -> trm_int 0)) in
    let mindex = mindex !new_dims partial_indices in
    let new_access = trm_array_access (trm_var ~typ:(typ_ptr !elem_ty) !new_var) mindex in
    let tmp_var = trm_let (x, typ_ptr etyp) new_access in
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
    let other_indices = List.init (List.length dims) (fun _ -> Ast.new_var (fresh_var_name ())) in
    let indices = (arith_f new_index) :: (List.map trm_var other_indices) in
    let mindex = mindex !new_dims indices in
    let access = trm_array_access (trm_var ~typ:(typ_ptr !elem_ty) !new_var) mindex in
    let new_resource = List.fold_right (fun (i, d) acc ->
      (* FIXME: need to match inner loop ranges. *)
      Resource_formula.formula_group_range { index = i; start = trm_int 0; direction = DirUp; stop = d; step = trm_step_one () } acc
    ) (List.combine other_indices dims) Resource_formula.(formula_uninit_cell access) in
    new_body_instrs, Resource_contract.push_loop_contract_clause (Exclusive Preserves) (Resource_formula.new_anon_hyp (), new_resource) contract
  else
    new_body_instrs, contract
  in
  let new_body = trm_seq ~annot:body.annot new_body_instrs in
  trm_seq_nobrace_nomarks [
    trm_add_mark mark_alloc
      (Matrix_core.let_alloc !new_var !elem_ty !new_dims);
    trm_for ~contract:new_contract ~annot:t.annot range new_body;
    trm_add_mark mark_free
      (Matrix_trm.free (trm_var ~typ:(typ_ptr !elem_ty) !new_var));
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

(** [fission_on_as_pair]: split loop [t] into two loops

    [index]: index of the splitting point
    [t]: ast of the loop
    *)
let fission_on_as_pair (mark_loops : mark) (index : int) (t : trm) : trm * trm =
  let (l_range, t_seq, contract) = trm_inv
    ~error:"Loop_basic.fission_on: only simple loops are supported"
    trm_for_inv t
  in
  let tl, _ = trm_inv trm_seq_inv t_seq in
  let tl1, _, tl2 = Mlist.split_on_marks index tl in
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
        Var_map.filter (fun h _ -> Var_set.mem h linear_invariant_hyps) tl1_usage
      in
      let tl1_inv_reads, (* = Iro *) tl1_inv_writes (* = I' *) = Var_map.partition (fun _ res_usage ->
        match res_usage with
        | SplittedFrac | JoinedFrac -> true
        | _ -> false
      ) tl1_inv_usage in
      let resource_set_of_hyp_map (hyps: 'a Var_map.t) (resources: resource_item list): resource_item list =
        List.filter (fun (h, _) -> Var_map.mem h hyps) resources
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
          | Some (v, typ, init) -> Var_set.add v acc
          | None -> acc
        ) Var_set.empty tl1
      in
      let split_res_comm = List.filter (fun (h, formula) ->
          Var_set.disjoint (trm_free_vars formula) bound_in_tl1
        ) split_res_comm
      in

      let tl2_usage = Resources.compute_usage_of_instrs tl2 in
      let post_inst_usage = Resource_computation.used_set_to_usage_map (Resources.post_inst t_seq) in
      let usage_after_tl1 = Resource_computation.update_usage_map ~current_usage:tl2_usage ~extra_usage:post_inst_usage in
      let used_in_split_res_comm = Resource_set.used_vars (Resource_set.make ~linear:split_res_comm ()) in
      let tl1_ensured = List.filter (fun (x, f) ->
        match Var_map.find_opt x tl1_usage with
        | Some (Ensured | ArbitrarilyChosen) when
          Var_set.mem x used_in_split_res_comm ->
            failwith "The resources at split point depend on the variable %s created before in the sequence" (var_to_string x)
        | Some Ensured when
          Var_map.mem x usage_after_tl1 &&
          Var_set.disjoint (trm_free_vars f) bound_in_tl1 ->
            true
        | _ -> false
        ) split_res.pure
      in
      let middle_iter_contract = Resource_set.copy (Resource_set.make ~pure:tl1_ensured ~linear:split_res_comm ()) in

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
        loop_ghosts = contract.loop_ghosts;
        invariant = { contract.invariant with linear = tl2_inv_writes };
        parallel_reads = tl1_inv_reads @ contract.parallel_reads;
        iter_contract = {
          pre = { middle_iter_contract with pure = middle_iter_contract.pure @ contract.iter_contract.pre.pure };
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

(** [fission_on]: split loop [t] into two loops

    [index]: index of the splitting point
    [t]: ast of the loop
    *)
let fission_on (mark_loops : mark) (mark_between_loops : mark) (index : int) (t : trm) : trm =
  let (ta,tb) = fission_on_as_pair mark_loops index t in
  trm_seq_helper ~braces:false [ Trm ta; Mark mark_between_loops; Trm tb ]

(** [fission tg]: expects the target [tg] to point somewhere inside the body of the simple loop
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
let normalize_loop_step (step : trm) : trm =
  if trm_is_one step then
    trm_step_one ()
  else
    step

let same_loop_range
  (range1 : loop_range)
  (range2 : loop_range) : bool =
  are_same_trm range1.start range2.start &&
  (range1.direction = range2.direction) &&
  are_same_trm range1.stop range2.stop &&
  are_same_trm range1.step range2.step

let same_loop_index (a : loop_range) (b : loop_range) : bool =
  assert (a.index.namespaces = [] && b.index.namespaces = []);
  a.index.name = b.index.name

(* [t] is a sequence;
   [index] is the index of the first loop to fuse in seq [t].
   Its annotations are kept.
   if [upwards], [index + 1] is the index of the second loop to fuse.
   if [not upwards], [index - 1] is the index of the second loop to fuse.
  *)
let fusion_on (index : int) (upwards : bool) (t : trm) : trm =
  let instrs, result = trm_inv
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
      let resource_set_of_hyp_map (hyps: 'a Var_map.t) (resources: resource_item list): resource_item list =
        List.filter (fun (h, _) -> Var_map.mem h hyps) resources
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
    trm_seq ~annot:t.annot ?loc:t.loc ?result new_instrs
  | _ -> failwith "unreachable"

(** [fusion]: expects the target [tg] to point at a loop that is followed by another loop with the same range (start, stop, step).
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

(** [grid_enumerate index_and_bounds tg]: expects the target [tg] to point at a loop iterating over
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
  apply_at_target_paths (Loop_core.grid_enumerate_on index_and_bounds) tg

(** [unroll ~braces ~my_mark tg]: expects the target to point at a simple loop of the shape
    for (int i = a; i < a + C; i++) or for (int i = 0; i < C; i++)
      then it will move the instructions out of the loop by replacing
      the index i occurrence with a + j in and j in the second case where
      j is an integer in range from 0 to C.

    Assumption: Both a and C should be declared as constant variables. *)
let%transfo unroll ?(inner_braces : bool = false) ?(outer_seq_with_mark : mark  = no_mark) ?(subst_mark : mark = no_mark) (tg : target): unit =
  Trace.justif_always_correct ();
  Nobrace_transfo.remove_after (fun _ ->
    apply_at_target_paths (Loop_core.unroll_on inner_braces outer_seq_with_mark subst_mark) tg)


type empty_range_mode =
| Generate_if
| Arithmetically_impossible
| Produced_resources_uninit_after

(** [move_out_on trm_index t]: moves an invariant instruction just before loop [t],
    [trm_index] - index of that instruction on its surrouding sequence (just checks that it is 0),
    [t] - ast of the for loop.
  *)
let move_out_on (instr_mark : mark) (loop_mark : mark) (empty_range: empty_range_mode) (trm_index : int) (t : trm) : trm =
  if (trm_index <> 0) then failwith "Loop_basic.move_out: not targeting the first instruction in a loop (got %d instead)" trm_index;
  let error = "Loop_basic.move_out: expected for loop" in
  let (range, body, contract) = trm_inv ~error trm_for_inv t in
  let instrs, _ = trm_inv ~error trm_seq_inv body in
  let instr = Mlist.nth instrs 0 in
  let rest = Mlist.pop_front instrs in

  if !Flags.check_validity then begin
    if is_free_var_in_trm range.index instr then
      (* NOTE: would be checked by var ids anyway *)
      trm_fail instr "Loop_basic.move_out: instruction uses loop index";
    Resources.assert_dup_instr_redundant 0 (Mlist.length instrs - 1) body;

    begin match empty_range with
    | Generate_if -> ()
    | Arithmetically_impossible -> failwith "Arithmetically_impossible is not implemented yet"
    | Produced_resources_uninit_after ->
      if not contract.strict then failwith "Need the for loop contract to be strict";
      let instr_usage = Resources.usage_of_trm instr in
      let invariant_written_by_instr = List.filter (Resource_set.(linear_usage_filter instr_usage keep_written)) contract.invariant.linear in
      List.iter (fun (_, f) -> if not (Resource_formula.is_formula_uninit f) then trm_fail instr "The instruction cannot be moved out because it consumes resources that are not uninitialized after the loop (and the loop range could be empty)"
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
      let resources_after = Option.unsome ~error:"Loop_basic.move_out: requires computed resources" instr.ctx.ctx_resources_after in
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

let move_out_alloc_on (trm_index : int) (t : trm) : trm =
  if (trm_index <> 0) then failwith "not targeting the first instruction in a loop";
  let error = "expected for loop" in
  let (range, body, contract) = trm_inv ~error trm_for_inv t in
  let instrs, _ = trm_inv ~error trm_seq_inv body in
  let instr_count = Mlist.length instrs in
  if (instr_count < 2) then failwith "expected at least two instructions";
  let alloc_instr = Mlist.nth instrs 0 in
  let free_instr = Mlist.nth instrs (instr_count - 1) in
  let (_, rest) = Mlist.extract 1 (instr_count - 2) instrs in

  let error = "expected array allocation instr" in
  let (array_var, _, alloc_init) = trm_inv ~error trm_let_inv alloc_instr in
  let (_, dims) = trm_inv ~error Matrix_trm.alloc_uninit_inv alloc_init in
  let error = "expected free instr" in
  let _ = trm_inv ~error Matrix_trm.free_inv free_instr in

  if !Flags.check_validity then begin
    (* NOTE: would be checked by var ids anyway *)
    if is_free_var_in_trm range.index alloc_instr then
      trm_fail alloc_instr "allocation instruction uses loop index";
    if is_free_var_in_trm range.index free_instr then
      trm_fail free_instr "free instruction uses loop index";
    (* Resources.assert_dup_instr_redundant 0 (Mlist.length instrs - 1) body;
      --> We know that `free x; alloc x = ()`
      *)

    Trace.justif "instructions from following iterations are redundant with first iteration"
  end;

  let open Resource_formula in
  let contract = { contract with invariant = { contract.invariant with linear = (new_anon_hyp (), formula_uninit_matrix (trm_var array_var) dims) :: contract.invariant.linear }} in
  let loop = trm_for ~annot:t.annot ~contract range (trm_seq rest) in

  trm_seq_nobrace_nomarks [
    alloc_instr;
    loop;
    free_instr]

(** [move_out_alloc ~empty_range tg]: same as [move_out], but supports moving out an allocation
    instruction together with its corresponding deallocation (that must be at the end of the loop).

    TODO: generalize [move_out] and [move_out_alloc] to moving out the any first/last group of instrs (I1 and I2) at once, if (I2; I1) is a no-op.
  *)
let%transfo move_out_alloc (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    Target.iter (fun p ->
      Resources.required_for_check ();
      let i, p = Path.index_in_surrounding_loop p in
      apply_at_path (move_out_alloc_on i) p
  ) tg)

(** [unswitch tg]:  expects the target [tg] to point at an if statement with a constant condition
     (not dependent on loop index or local variables) inside a loop.  Then it will take that
      if statment outside the loop.

   @correctness: requires that the loop is parallelizable *)
let%transfo unswitch (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    Target.iter (fun p ->
      let i, p = Path.index_in_surrounding_loop p in
      Target.apply_at_path (Loop_core.unswitch_at i) p
    ) tg
  )


(** [to_unit_steps index tg]: expects target [tg] to point at a for loop
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
  apply_at_target_paths (Loop_core.to_unit_steps_on index) tg

(** [fold ~direction index start stop step tg]: expects the target [tg] to point at the first instruction in a sequence
    and it assumes that the sequence containing the target [tg] is composed of a list of instructions which
    can be expressed into a single for loop with [index] [direction] [start] [nb_instructions] and [step] as loop
    components. *)
let%transfo fold ~(index : string) ~(start : int) ~(step : int) (tg : target) : unit =
  apply_at_target_paths (Loop_core.fold_at index start step) tg

(** [split_range nb cut tg]: expects the target [tg] to point at a simple loop
    then based on the arguments nb or cut it will split the loop into two loops. *)
let%transfo split_range ?(nb : int = 0) ?(cut : trm = trm_unit())
  ?(mark_loop1 : mark = no_mark) ?(mark_loop2 : mark = no_mark)
  ?(mark_simpl: mark = no_mark) (tg : target) : unit =
  Nobrace_transfo.remove_after( fun _ ->
    apply_at_target_paths (Loop_core.split_range_at nb cut mark_loop1 mark_loop2 mark_simpl) tg;
    Trace.justif "split point is pure and will be proved to belong in loop range"
  )

type shift_kind =
| ShiftBy of trm
| StartAtZero
| StartAt of trm
| StopAt of trm

let shift_kind_to_string = function
| ShiftBy t -> "ShiftBy " ^ (Ast_to_c.ast_to_string t)
| StartAtZero -> "StartAtZero"
| StartAt t -> "StartAt " ^ (Ast_to_c.ast_to_string t)
| StopAt t -> "StopAt " ^ (Ast_to_c.ast_to_string t)

let ghost_group_shift = toplevel_var "group_shift"
let ghost_ro_group_shift = toplevel_var "ro_group_shift"
let ghost_group_unshift = toplevel_var "group_unshift"
let ghost_ro_group_unshift = toplevel_var "ro_group_unshift"

(** transforms a loop index (e.g. shift, scale). *)
let transform_range_on
 (new_range : loop_range -> var -> loop_range * trm * 'a)
 (to_prove : trm list)
 (pre_res_trans : loop_range -> loop_range -> 'a -> resource_item list -> trm list)
 (post_res_trans : loop_range -> loop_range -> 'a -> resource_item list -> trm list)
 (next_inv_trans : loop_range -> loop_range -> 'a -> resource_set -> mark -> trm list)
 (new_index : string)
 (mark_let : mark) (mark_for : mark) (mark_contract_occs : mark)
 (t : trm) : trm =
 let index' = new_var new_index in
 let error = "expected a target to a simple for loop" in
 let (range, body_terms, contract) = trm_inv ~error trm_for_inv_instrs t in
 let (range', index_expr, data) = new_range range index' in
 (* NOTE: assuming int type if no type is available *)
 let body_terms' =
   Mlist.push_front (trm_add_mark mark_let (trm_let (range.index, (Option.value ~default:(typ_int) range.start.typ)) index_expr)) (
   Mlist.push_front (Resource_trm.(Resource_formula.(assume (formula_in_range (trm_var range.index) (formula_loop_range range))))) (
   body_terms))
 in
 begin if not contract.strict then
   trm_add_mark mark_for (trm_for_instrs ~annot:t.annot range' body_terms')
   (* trm_fail t "expected loop contract when checking validity" ? *)
 else
   let pre_ghosts = pre_res_trans range range' data contract.iter_contract.pre.linear in
   let post_ghosts = post_res_trans range range' data contract.iter_contract.post.linear in
   let contract' = Resource_contract.loop_contract_subst (Var_map.singleton range.index (trm_add_mark mark_contract_occs index_expr)) contract in
   let with_index_start range contract = Resource_set.subst_var range.index range.start (Resource_set.filter_with_var range.index contract.invariant) in
   let with_index_stop range contract = Resource_set.subst_var range.index range.stop (Resource_set.filter_with_var range.index contract.invariant) in
   let start_inv_ghost = Resource_trm.ghost_admitted {
    pre = with_index_start range contract;
    post = with_index_start range' contract';
   } in
   let stop_inv_ghost = Resource_trm.ghost_admitted {
    pre = with_index_stop range' contract';
    post = with_index_stop range contract;
   } in
   let next_inv_ghost = next_inv_trans range range' data contract.invariant mark_contract_occs in
   let body_terms' = Mlist.merge body_terms' (Mlist.of_list next_inv_ghost) in
   trm_seq_nobrace_nomarks (to_prove @ pre_ghosts @ [
    start_inv_ghost;
    trm_add_mark mark_for (trm_for_instrs ~annot:t.annot ~contract:contract' range' body_terms');
    stop_inv_ghost
   ] @ post_ghosts)
 end

let shift_range_on (kind : shift_kind) =
  let new_range { index; start; direction; stop; step } index' =
    (* spec:
      let start' = trm_add start shift in
      let stop' = trm_add stop shift in *)
    let (shift, start', stop') = match kind with
    (* spec:
      let start' = trm_add start shift in
      let stop' = trm_add stop shift in *)
    | ShiftBy s -> (s, trm_add_int start s, trm_add_int stop s)
    (* NOTE: assuming int type *)
    | StartAtZero -> (trm_minus ~typ:typ_int start, trm_int 0, trm_sub_int stop start)
    | StartAt v -> (trm_sub_int v start, v, trm_add_int stop (trm_sub_int v start))
    | StopAt v -> (trm_sub_int v stop, trm_add_int start (trm_sub_int v stop), v)
    in
    let range' = { index = index'; start = start'; direction; stop = stop'; step } in
    let index_expr = trm_sub_int (trm_var ~typ:typ_int index') shift in
    (range', index_expr, shift)
  in
  let open Resource_formula in
  let shift_ghosts ghost ghost_ro r r' shift =
    List.map (fun (_, formula) ->
      (* TODO: factorize generic ghost mode code for all transfos *)
      let ghost, formula, extra_params = match formula_read_only_inv formula with
      | Some f -> ghost_ro, f.formula, ["f", f.frac]
      | None -> ghost, formula, []
      in
      let i = new_var r.index.name in
      let items = formula_fun [i, typ_int] (trm_subst_var r.index (trm_var ~typ:typ_int i) formula) in
      Resource_trm.ghost (ghost_call ghost ([
        "start", r.start; "stop", r.stop; "step", r.step; "items", items;
        "shift", shift; "new_start", r'.start; "new_stop", r'.stop
      ] @ extra_params))
    )
  in
  let to_prove = [] in
  let pre_res_trans = shift_ghosts ghost_group_shift ghost_ro_group_shift in
  let post_res_trans = shift_ghosts ghost_group_unshift ghost_ro_group_unshift in
  let next_inv_trans r r' shift inv mark_contract_occs =
    let inv_with_index = Resource_set.filter_with_var r.index inv in
    if Resource_set.is_empty inv_with_index then [] else [
      Resource_trm.ghost_admitted {
        pre = Resource_set.subst_var r.index (trm_add_mark mark_contract_occs (trm_add_int (trm_sub_int (trm_var r'.index) shift) r'.step)) inv_with_index;
        post = Resource_set.subst_var r.index (trm_sub_int (trm_add_int (trm_var r'.index) r'.step) shift) inv_with_index;
      }]
  in
  transform_range_on new_range to_prove pre_res_trans post_res_trans next_inv_trans

(** [shift_range index kind]: shifts a loop index range according to [kind], using a new [index] name.

  validity: expressions used in shifting must be referentially transparent, other expressions can be bound before using [Sequence.insert].
  *)
let%transfo shift_range (index : string) (kind : shift_kind)
  ?(mark_let : mark = no_mark)
  ?(mark_for : mark = no_mark)
  ?(mark_contract_occs : mark = no_mark)
  (tg : target) : unit =
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
  Nobrace_transfo.remove_after (fun () ->
    Target.apply_at_target_paths (shift_range_on kind index mark_let mark_for mark_contract_occs) tg)

let ghost_group_scale = toplevel_var "group_scale"
let ghost_ro_group_scale = toplevel_var "ro_group_scale"
let ghost_group_unscale = toplevel_var "group_unscale"
let ghost_ro_group_unscale = toplevel_var "ro_group_unscale"

let scale_range_on (factor : trm) =
  let new_range { index; start; direction; stop; step } index' =
    let new_start =
      match trm_lit_inv start with
      | Some (Lit_int (_, 0)) -> start
      | _ -> trm_fail start "scale_range: only start at zero is supported."
    in
    let new_step =
      match trm_lit_inv step with
      | Some (Lit_int (_, 1)) -> factor
      | _ -> trm_mul_int factor step
    in
    let new_stop = match direction with
      | DirUp -> (trm_mul_int factor stop)
      | DirUpEq | DirDown | DirDownEq -> failwith "scale_range: only up to loops are supported."
    in
    let range' = { index = index'; start = new_start; direction; stop = new_stop; step = new_step } in
    let index_expr = trm_exact_div ~typ:typ_int (trm_var ~typ:typ_int index') factor in
    (range', index_expr, ())
  in
  let open Resource_formula in
  let to_prove = [Resource_trm.to_prove (formula_neq ~typ:typ_int factor (trm_int ~typ:typ_int 0))] in
  let scale_ghosts ghost ghost_ro r r' () =
    List.map (fun (_, formula) ->
      (* TODO: factorize generic ghost mode code for all transfos *)
      let ghost, formula = match formula_read_only_inv formula with
      | Some { formula } -> ghost_ro, formula (* FIXME: wrong fraction splits *)
      | None -> ghost, formula
      in
      let i = new_var r.index.name in
      let items = formula_fun [i, typ_int] (trm_subst_var r.index (trm_var i) formula) in
      Resource_trm.ghost (ghost_call ghost [
        "stop", r.stop; "step", r.step; "items", items;
        "factor", factor; "new_step", r'.step; "new_stop", r'.stop])
    )
  in
  let pre_res_trans = scale_ghosts ghost_group_scale ghost_ro_group_scale in
  let post_res_trans = scale_ghosts ghost_group_unscale ghost_ro_group_scale in  let next_inv_trans r r' () inv mark_contract_occs =
    (* TODO *)
    []
  in
  transform_range_on new_range to_prove pre_res_trans post_res_trans next_inv_trans

(** [scale_range index factor tg]: expects target [tg] to point at a for loop
  [index]. [factor] denotes the factor by which indices are multiplied.

  validity: factor must be a pure expression, and cannot be equal to 0
    *)
let%transfo scale_range (index : string) (factor : trm)
 ?(mark_let : mark = no_mark)
 ?(mark_for : mark = no_mark)
 ?(mark_contract_occs : mark = no_mark)
 (tg : target) : unit =
 if !Flags.check_validity then begin
    if Resources.trm_is_pure factor then
      (* TODO: also works for read-only *)
      Trace.justif "scaling by a pure factor is correct when proving that factor != 0"
    else
      trm_fail factor "scaling by a non-pure expression is not yet supported, requires checking that expression is read-only, introduce a binding with 'Sequence.insert' to workaround" (* TODO: combi doing this *)
 end;
 Nobrace_transfo.remove_after (fun () ->
  apply_at_target_paths (scale_range_on factor index mark_let mark_for mark_contract_occs) tg
 )

let simplify_ghost_group_scale_on_opt (t : trm) : trm option =
  let open Option.Monad in
  let* ghost_call = Resource_trm.ghost_call_inv t in
  let* gv = trm_var_inv ghost_call.ghost_fn in
  let is_group_scale = var_eq gv ghost_group_scale ||
    var_eq gv ghost_ro_group_scale ||
    var_eq gv ghost_group_unscale ||
    var_eq gv ghost_ro_group_unscale
  in
  if not is_group_scale then None else begin
    let arg_is name (arg, _) = arg.name = name in
    let* (_, factor) = List.find_opt (arg_is "factor") ghost_call.ghost_args in
    let* (_, stop) = List.find_opt (arg_is "stop") ghost_call.ghost_args in
    let* (_, step) = List.find_opt (arg_is "step") ghost_call.ghost_args in
    let* (_, new_stop) = List.find_opt (arg_is "new_stop") ghost_call.ghost_args in
    let* (_, new_step) = List.find_opt (arg_is "new_step") ghost_call.ghost_args in
    if is_trm_int 1 factor then Some (trm_seq_nobrace_nomarks []) else begin
      if are_same_trm stop new_stop && are_same_trm step new_step
      then Some (trm_seq_nobrace_nomarks [])
      else None
    end
  end

let%transfo simplify_all_ghosts_group_scale (tg : target) : unit =
  Trace.justif_always_correct ();
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    Target.apply_at_path (trm_bottom_up (fun t ->
      match simplify_ghost_group_scale_on_opt t with
      | Some t2 -> t2
      | None -> t
    )) p
  ) tg)

type extension_kind =
| ExtendNothing
| ExtendToZero
| ExtendTo of trm
| ExtendBy of trm

let extension_kind_to_string = function
| ExtendNothing -> "ExtendNothing"
| ExtendToZero -> "ExtendToZero"
| ExtendTo t -> "ExtendTo " ^ (Ast_to_c.ast_to_string t)
| ExtendBy t -> "ExtendBy " ^ (Ast_to_c.ast_to_string t)

let extend_range_on (start_extension : extension_kind) (stop_extension : extension_kind) (t : trm) : trm =
  let error = "Loop_basic.extend_range_on: expected a target to a simple for loop" in
  let ({ index; start; direction; stop; step }, body, _contract) = trm_inv ~error trm_for_inv t in
  assert (direction = DirUp);
  (* TODO: does it work in other cases?
     assert (trm_is_one step); *)
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
  let if_before_stop body = trm_seq_nomarks [make_if (trm_lt ~typ:typ_int (trm_var index) stop) body] in
  let if_after_start body = trm_seq_nomarks [make_if (trm_le ~typ:typ_int start (trm_var index)) body] in
  let (stop', body') = begin match stop_extension with
  | ExtendNothing -> (stop, body)
  | ExtendToZero -> failwith "not implemented yet"
  | ExtendTo v -> (v, if_before_stop body)
  | ExtendBy v -> (trm_add_int stop v, if_before_stop body)
  end in
  let (start', body'') = begin match start_extension with
  | ExtendNothing -> (start, body')
  | ExtendToZero -> (trm_int 0, if_after_start body')
  | ExtendTo v -> (v, if_after_start body')
  | ExtendBy v -> (trm_sub_int start v, if_after_start body')
  end in
  trm_for ~annot:t.annot { index; start = start'; direction; stop = stop'; step } body''

(** [extend_range]: extends the range of a loop on [lower] and/or [upper] bounds.
   The body of the loop is guarded by ifs statements, doing nothing on the extension points.

   For this to be correct, the loop bounds must be extended, not shrinked.
  *)
let%transfo extend_range ?(start : extension_kind = ExtendNothing) ?(stop : extension_kind = ExtendNothing) (tg : target) : unit =
  Target.apply_at_target_paths (extend_range_on start stop) tg

(** [rename_index new_index]: renames the loop index variable *)
let%transfo rename_index (new_index : string) (tg : target) : unit =
  apply_at_target_paths (Loop_core.rename_index_on new_index) tg;
  Trace.justif "renaming loop index is always correct (unique ids avoid name conflicts)"

(* FIXME: duplicated code from tiling. *)
let slide_on (tile_index : string) (bound : tile_bound) (tile_size : trm) (tile_step : trm) (t : trm) : trm =
  let error = "Loop_basic.slide_on: only simple loops are supported." in
  let ({ index; start; direction; stop; step }, body, _contract) = trm_inv ~error trm_for_inv t in
  let tile_index = new_var (Tools.string_subst "${id}" index.name tile_index) in
  let tile_bound =
   if trm_is_one step then trm_add_int (trm_var tile_index) tile_size else trm_add_int (trm_var tile_index) (trm_mul_int tile_size step) in
  let inner_loop =
  begin match bound with
  | TileBoundMin ->
    let tile_bound =
    trm_apps (trm_var (name_to_var "min")) [stop; tile_bound] in
    trm_for { index; start = trm_var tile_index; direction; stop = tile_bound; step } body
  | TileDivides ->
    trm_for { index; start = trm_var tile_index; direction; stop = tile_bound; step } body
  | TileBoundAnd ->
    let init = trm_let_mut (index, typ_int) (trm_var tile_index) in
    let cond = trm_and (trm_ineq direction (trm_var_get index)
      (if trm_is_one step
        then (trm_add_int (trm_var tile_index) tile_size)
        else (trm_add_int (trm_var tile_index) (trm_mul_int tile_size step) ))) (trm_ineq direction (trm_var_get index) stop)
      in
    let step = if trm_is_one step then trm_post_incr (trm_var index)
      else trm_compound_assign ~typ:typ_int Binop_add (trm_var index) step in
    let new_body = trm_subst_var index (trm_var_get index) body in
    trm_for_c init cond step new_body
  end in
  (* NOTE: only outer loop differs from tiling? *)
  let may_scale x = if trm_is_one step
    then x else trm_mul_int x step in
  let outer_loop_step = may_scale tile_step in
  let outer_stop = (trm_add_int stop (may_scale (trm_sub_int tile_step tile_size))) in
  let outer_loop =
      trm_for { index = tile_index; start; direction; stop = outer_stop; step = outer_loop_step } (trm_seq_nomarks [inner_loop])
  in
  trm_pass_labels t outer_loop

(** [slide]: like [tile] but with the addition of a [step] parameter that controls how many iterations stand between the start of two tiles. Depending on [step] and [size], some iterations may be discarded or duplicated.
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
      if Mlist.is_empty body && Resources.trm_is_pure range.start && Resources.trm_is_pure range.stop && Resources.trm_is_pure range.step
      (* TODO: No need to check pure range if this is a global invariant of Trm_for *)
      (* 2. delete *)
      then Some (Sequence_core.delete_at (Dir.span_around i) t_seq)
      else None
    ))

(** [delete_void]: deletes a loop with empty body. *)
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

(** [delete_if_void]: deletes a loop if it has an empty body. *)
let%transfo delete_if_void (tg : target) : unit =
  Trace.justif_always_correct ();
  Target.iter (fun p ->
    let (i, p_seq) = Path.index_in_seq p in
    apply_at_path (fun t_seq ->
      match delete_void_on i t_seq with
      | Some t -> t
      | None -> t_seq
    ) p_seq
  ) tg

(** [loop_single_on i t] : Expect an index and a sequence trm t, i-th trm of the
    sequence should point to a var def.
    Replace
      int k = ..
      tl
    with
      for(int k = .. ; k < .. +1 ; k++)
      {
        tl
      }

*)
let loop_single_on (i : int) (t : trm) : trm =
  let seq, result =
    trm_inv ~error:"Loop_single_on: Expected the target to be part of a Seq"
      trm_seq_inv t
  in
  let let_stmt = Mlist.nth seq i in
  let var, typ, value =
    trm_inv
      ~error:
        "Loop_single_on: Expected the target to be a let operation (int .. = ...;)"
      trm_let_inv let_stmt
  in
  let tl1, _mark, tl2 = Mlist.split_on_marks i seq in

  let l_range : loop_range =
    {
      index = var;
      start = value;
      stop = trm_add_int value (trm_int 1);
      direction = DirUp;
      step = trm_step_one ();
    }
  in

  match result with
  | Some res_expr ->
      let mut_res_var = new_var "res" in
      let mut_res = trm_let_mut_uninit (mut_res_var, typ_int) in
      let new_body =
        trm_seq_helper
          [
            TrmMlist (Mlist.pop_front tl2);
            Trm (trm_set (trm_var mut_res_var) (trm_var res_expr));
          ]
      in
      let loop = trm_for l_range new_body in
      let result_v = new_var "__res" in
      let result_p = trm_let (result_v, typ_int) (trm_var_get mut_res_var) in
      trm_seq_helper ~result:result_v
        [ TrmMlist (Mlist.push_back mut_res tl1); Trm loop; Trm result_p ]
  | None ->
      let loop = trm_for l_range (trm_seq (Mlist.pop_front tl2)) in
      trm_seq_helper [ TrmMlist tl1; Trm loop ]

let%transfo loop_single (tg : target) : unit = (apply_at_target_paths_in_seq loop_single_on) tg

(** [elim_loop_single_on t]: Reverse the transformation loop_single_on.
    Expects t to be a trm_for whose lower bound is trm and upper bound is trm + 1.
    Replaces the for loop with a let binding and removes the loop.
    Transform:
      for(int i = k; i <k + 1 ; k++){
      tl
      }
    into:
      const int i = k;
      tl
*)
let elim_loop_single_on (t : trm) : trm =
  let error = "elim_loop_single_on: Expect the target to point to a for loop" in
  let l_range, body, _contract = trm_inv ~error trm_for_inv_instrs t in
  if l_range.direction <> DirUp then
    trm_fail t "elim_loop_single_on: Expect the direction to be DirUp";
  if not (are_same_trm l_range.step (trm_step_one ())) then
    trm_fail t "elim_loop_single_on: Expect a step of one for the loop";
  if not (are_same_trm (trm_add_int l_range.start (trm_int 1)) l_range.stop)
  then trm_fail t "elim_loop_single_on: Expected a loop with a unique step";
  let index_start = trm_let (l_range.index, typ_int) l_range.start in
  trm_seq_nobrace_nomarks
    (Mlist.to_list(Mlist.push_front index_start body))

(** [elim_loop_single tg]: Expects the target to point to a for loop. Applies
    [ elim_loop_single_on] *)
let%transfo elim_loop_single (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
      apply_at_target_paths elim_loop_single_on tg)

(** [if_loop_switch t]: Expects t to point to a trm_for
  The trm for as to be immediatly followed by a trm_if; the only term of the seq.
  The cond in trm_if has to be a binop_eq.
  Transform :
{
    for(int i = a ; i < b ; i++)
      {
      if (a == c)
        seq
      }
}
  into:
{
      for (int i = c ; i < c+1; i++){
      if(i>= a && i < b )
        seq
      }
 }     *)

let if_loop_switch_on (t : trm) : trm =
  let error = "if_loop_switch: Expected a for loop" in
  let l_range, body, contract = trm_inv ~error trm_for_inv t in
  let error = "if_loop_switch: Expected instrs inside the body" in
  let instrs, res = trm_inv ~error trm_seq_inv body in
  let error = "if_loop_switch: Expected if as the first instr" in
  if Mlist.length instrs <> 1 then
    trm_fail t
      "if_loop_switch: Expected exactly one if statement inside the for loop";
  let cond, then_, else_ =
    trm_inv ~error trm_if_inv (List.nth (Mlist.to_list instrs) 0)
  in
  let error = "if_loop_switch: Expected an equality condition" in
  let l_value, r_value = trm_inv ~error trm_eq_inv cond in
  let error =
    "if_loop_switch: Expected a var as the left part of the equality condition"
  in
  let l_value_var = trm_inv ~error trm_var_inv l_value in

  if not (var_eq l_value_var l_range.index) then
    trm_fail t
      "if_loop_switch: Expected same trm for loop index and if condition left \
       value";
  match l_value.typ with
  | None -> trm_fail t "if_loop_switch: The index should be typed "
  | Some typ ->
      let new_cond =
        trm_and
          (trm_ge ~typ l_value l_range.start)
          (trm_lt ~typ l_value l_range.stop)
      in
      let new_if = trm_if new_cond then_ else_ in
      let new_lrange : loop_range =
        {
          index = l_range.index;
          start = r_value;
          stop = trm_add_int r_value (trm_int 1);
          direction = DirUp;
          step = trm_step_one ();
        }
      in
      let new_instrs = Mlist.replace_at 0 new_if instrs in
      trm_for new_lrange (trm_seq ?result:res new_instrs) ~contract

let%transfo if_loop_switch (tg : target) = apply_at_target_paths if_loop_switch_on tg


(** [reverse_comp ~lhs ~binop ~rhs t] checks if [t] equals [lhs] or [rhs], and
    returns the comparison in the form (side with [t], op, other side), flipping
    the operator if needed. Fails if [t] matches neither. *)
let reverse_comp ~(lhs : trm) ~(binop : binary_op) ~(rhs : trm) t : trm * binary_op * trm =
  if are_same_trm lhs t then (lhs, binop, rhs)
  else if are_same_trm rhs t then (rhs, reverse_comp_binop binop, lhs)
  else
    trm_fail t
      "reverse_comp: Did not find the trm t as a trm of the comparaison"



(** [if_loop_switch_gen_on t] transforms a for-loop with an if-condition into a
    for-loop with adjusted bounds based on that condition. *)

let refactor_if_in_loop_on (t : trm) : trm =
  let error = "refactor_if_in_loop: Expected a for loop" in
  let l_range, body, contract = trm_inv ~error trm_for_inv t in
  let error = "refactor_if_in_loop: Expected a sequence of instructions inside the loop body"
  in
  let instrs, res = trm_inv ~error trm_seq_inv body in
  if Mlist.length instrs > 1 then
    trm_fail t "refactor_if_in_loop: Expected exactly the pattern 'for-if'";
  let new_start = ref l_range.start in
  let new_stop = ref l_range.stop in
  let rec aux (t : trm) : unit =
    match trm_and_inv t with
    | Some (t1, t2) ->
        aux t1;
        aux t2
    | _ -> (
        let error = "refactor_if_in_loop: Expected a comparaison trm" in
        let lhs, binop, rhs = trm_inv ~error trm_extract_binop_inv t in
        let index, binop, to_comp =
          reverse_comp ~lhs ~binop ~rhs (trm_var l_range.index)
        in
        match binop with
        | Binop_ge -> new_start := trm_max to_comp !new_start
        | Binop_gt ->
            new_start := trm_max (trm_add_int to_comp (trm_int 1)) !new_start
        | Binop_le ->
            new_stop := trm_min (trm_add_int to_comp (trm_int 1)) !new_stop
        | Binop_lt -> new_stop := trm_min to_comp !new_stop
        | Binop_eq ->
            new_start := to_comp;
            new_stop := trm_add_int to_comp (trm_int 1)
        | _ ->
            trm_fail t
              "refactor_if_in_loop: Expected comparison using one of the \
               following operators: <=, <, ==, >, >=")
  in
  let error = "refactor_if_in_loop: Expected exactly a for-if pattern" in
  let cond, then_, else_ = trm_inv ~error trm_if_inv (Mlist.nth instrs 0) in
  aux cond;
  trm_for
    {
      start = !new_start;
      stop = !new_stop;
      step = l_range.step;
      index = l_range.index;
      direction = l_range.direction;
    }
    then_ ~contract

let%transfo refactor_if_in_loop (tg : target) =
  apply_at_target_paths refactor_if_in_loop_on tg
