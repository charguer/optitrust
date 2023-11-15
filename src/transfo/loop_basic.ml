open Prelude
open Target

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
    apply_on_targets (Loop_core.tile index bound tile_size) tg
  )

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
(* LATER/ deprecated *)
let hoist_old ?(name : string = "${var}_step") ?(array_size : trm option) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
    apply_on_transformed_targets (Path.index_in_surrounding_loop)
     (fun t (i, p) -> Loop_core.hoist_old name i array_size t p) tg)

(* TODO: clean up code *)
let hoist_on (name : string)
             (mark : mark option)
             (arith_f : trm -> trm)
             (decl_index : int) (t : trm) : trm =
  let error = "Loop_basic.hoist_on: only simple loops are supported" in
  let (range, body, contract) = trm_inv ~error trm_for_inv t in
  let (index, start, dir, stop, step, _par) = range in
  assert (dir = DirUp); (* TODO: other directions *)
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
  | _ -> fail t.loc "Loop_basic.hoist_on: unsupported loop step"
  in
  let body_instrs = trm_inv ~error trm_seq_inv body in
  let elem_ty = ref (typ_auto()) in
  let old_var = ref dummy_var in
  let new_var = ref dummy_var in
  let new_dims = ref [] in
  let new_access = ref trm_dummy in
  let with_mindex (dims : trms) : trm =
    new_dims := (arith_f array_size) :: dims;
    let partial_indices = (arith_f new_index) ::
      (List.init (List.length dims) (fun _ -> trm_lit (Lit_int 0))) in
    mindex !new_dims partial_indices
  in
  let update_decl (decl : trm) : trm =
    let error = "Loop_basic.hoist_on: expected variable declaration with MALLOCN initialization" in
    let (x, dims, etyp, elem_size) = trm_inv ~error Matrix_core.let_alloc_inv_with_ty decl in
    old_var := x;
    new_var := Trm.new_var (Tools.string_subst "${var}" x.name name);
    elem_ty := etyp;
    let mindex = with_mindex dims in
    new_access := trm_array_access (trm_var !new_var) mindex;
    trm_let Var_immutable (x, typ_const_ptr etyp) !new_access
  in
  let body_instrs_new_decl = Mlist.update_nth decl_index update_decl body_instrs in
  let new_body_instrs = begin
    let free_index_opt = ref None in
    Mlist.iteri (fun i instr ->
      match Matrix_core.free_inv instr with
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
    | None -> fail body.loc "Loop_basic.hoist: expected free instruction"
  end in
  let new_body_instrs, new_contract = match contract with
  | Some contract ->
    let new_resource = Resource_formula.(formula_model !new_access trm_cell) in
    new_body_instrs, Some (Resource_contract.push_loop_contract_clause Modifies (None, new_resource) contract)
  | None ->
    (* TODO: Generate ghost focus *)
    new_body_instrs, None
  in
  let new_body = trm_seq ~annot:body.annot new_body_instrs in
  trm_seq_nobrace_nomarks [
    trm_may_add_mark mark
      (Matrix_core.let_alloc_with_ty !new_var !new_dims !elem_ty);
    trm_for ?contract:new_contract ~annot:t.annot range new_body;
    Matrix_core.free !new_dims (trm_var !new_var);
  ]

(* TODO: document *)
let%transfo hoist ?(name : string = "${var}_step")
          ?(mark : mark option)
          ?(arith_f : trm -> trm = Arith_core.(simplify_aux true gather_rec))
         (tg : target) : unit =
  Trace.justif_always_correct ();
  Nobrace_transfo.remove_after (fun _ ->
    Target.apply (fun t p_instr ->
      let (i, p) = Path.index_in_surrounding_loop p_instr in
      Path.apply_on_path (hoist_on name mark arith_f i) t p
      ) tg)

(* [fission_on_as_pair]: split loop [t] into two loops

    [index]: index of the splitting point
    [t]: ast of the loop
    *)
let fission_on_as_pair (index : int) (t : trm) : trm * trm =
  let (l_range, tl, contract) = trm_inv
    ~error:"Loop_basic.fission_on: only simple loops are supported"
    trm_for_inv_instrs t
  in
  (* TODO: Repair contract *)
  let tl1, tl2 = Mlist.split index tl in
  let fst_contract = ref None in
  let snd_contract = ref None in
  begin match contract with
  | _ when not !Flags.check_validity -> ()
  | None -> fail t.loc "Loop_basic.fission_on: requires an annotated for loop to check validity"
  | Some contract ->
    let open Resource_formula in

    if (Mlist.is_empty tl1) then begin
      fst_contract := Some Resource_contract.empty_loop_contract;
      snd_contract := Some contract
    end else if (Mlist.is_empty tl2) then begin
      fst_contract := Some contract;
      snd_contract := Some Resource_contract.empty_loop_contract;
    end else begin
      let first_tl2_instr = Mlist.nth tl2 0 in

      let linear_invariant = contract.invariant.linear in
      let linear_hyps = Var_set.of_list (List.map (fun (h, _) -> h) linear_invariant) in
      let linear_invariant_usage res_usage =
        Hyp_map.filter (fun h _ -> Var_set.mem h linear_hyps) res_usage
      in
      let tl1_inv_res = linear_invariant_usage (Resources.compute_usage_of_instrs (Mlist.to_list tl1)) in
      let tl2_inv_res = linear_invariant_usage (Resources.compute_usage_of_instrs (Mlist.to_list tl2)) in
      begin if linear_invariant = [] then
        Trace.justif "The for loop is parallelizable"
      else
        (* TODO: Also assert that the loop index is not used in invariants *)
        Resources.assert_usages_commute t.loc tl1_inv_res tl2_inv_res;
        Trace.justif "The instructions around split point commute with respect to the loop invariant resources"
      end;

      let error = "Loop_basic.fission_on: expected resources to be computed" in
      let tl2_ctx_res = unsome_or_fail first_tl2_instr.loc error (first_tl2_instr.ctx.ctx_resources_before) in
      let bound_in_ghosts = List.fold_left (fun acc (g, _) -> (* TODO: gather_vars_in_contract *)
          Var_set.add g acc
        ) Var_set.empty contract.loop_ghosts
      in
      let bound_in_tl1 = Mlist.fold_left (fun acc ti -> (* TODO: gather bound_vars_in_trms *)
          match trm_let_inv ti with
          | Some (vk, v, typ, init) -> Var_set.add v acc
          | None -> acc
        ) Var_set.empty tl1
      in
      (* At the fission point, we remove resources mentioning variables that were bound
        before the fission point, or that were bound by the contract ghost bindings *)
      let filter_resource_items its =
        List.filter (fun (h, formula) ->
          printf "h: %s\n" (var_to_string h);
          not (Var_set.mem h linear_hyps) &&
          not (Var_set.mem h bound_in_ghosts) &&
          Var_set.disjoint (trm_free_vars formula) bound_in_tl1
        ) its
      in
      printf "linear hyps: %s\n" (vars_to_string (Var_set.elements linear_hyps));
      let filter_resource_set res =
        { pure = filter_resource_items res.pure;
          linear = filter_resource_items res.linear;
          fun_specs = Var_map.empty }
      in
      let resources_after_tl1 = filter_resource_set tl2_ctx_res in
      fst_contract := Some {
        loop_ghosts = contract.loop_ghosts;
        invariant = contract.invariant;
        iter_contract = {
          pre = contract.iter_contract.pre;
          post = resources_after_tl1;
        }
      };
      snd_contract := Some {
        loop_ghosts = contract.loop_ghosts;
        invariant = contract.invariant;
        iter_contract = {
          pre = resources_after_tl1;
          post = contract.iter_contract.post;
        }
      };
    end;
  end;
  let ta = trm_for_instrs ?contract:!fst_contract l_range tl1 in
  let tb = trm_copy (trm_for_instrs ?contract:!snd_contract l_range tl2) in
  (ta, tb)
    (* Note: the trm_copy is needed because the loop index in the
       two loops must have a different id. We copy the second loop
       because fission_all_instr process them from the end. *)

(* [fission_on]: split loop [t] into two loops

    [index]: index of the splitting point
    [t]: ast of the loop
    *)
let fission_on (index : int) (t : trm) : trm =
  let (ta,tb) = fission_on_as_pair index t in
  trm_seq_nobrace_nomarks [ ta; tb ]


(* [fission_all_instrs_on]: split loop [t] into N loops,
   one per instruction in the loop body

   [indices]: list of indices of positions where to split
   [t]: ast of the loop
*)

let fission_all_instrs_on (indices : int list) (t : trm) : trm =
  let rec aux (indices : int list) (t : trm) : trm list =
    match indices with
    | [] -> [t]
    | i::indices' ->
        let (ta,tb) = fission_on_as_pair i t in
        let tl = aux indices' ta in
        tb :: tl
    in
  let tl = aux (List.rev indices) t in
  trm_seq_nobrace_nomarks (List.rev tl)


(* [fission tg]: expects the target [tg] to point somewhere inside the body of the simple loop
   It splits the loop in two loops, the spliting point is trm matched by the relative target.

   @correctness: Reads in new second loop need to never depend on writes on
   first loop after index i. Writes in new second loop need to never overwrite
   writes in first loop after index i. *)
let%transfo fission (tg : target) : unit =
  Resources.required_for_check ();
  Nobrace_transfo.remove_after (fun _ ->
    Target.apply (fun t p_before ->
      let (p_seq, split_i) = Path.last_dir_before_inv_success p_before in
      let p_loop = Path.parent_with_dir p_seq Dir_body in
      apply_on_path (fission_on split_i) t p_loop)
    tg);
  Transfo_debug.current_ast ~style:ContractDisplay "SJD";
  Resources.justif_correct "loop resources where successfully split"

(* [fission_all_instrs]: similar to [fission],
   but splits the targeted loop into N loops,
   one per instruction in the loop body.
   or at specific indices.
   *)
let%transfo fission_all_instrs ?(indices : int list option) (tg : target) : unit =
  Resources.required_for_check ();
  Nobrace_transfo.remove_after (fun _ ->
    let aux t =
      let _, tl, _ = trm_inv ~error:"fission_all_instrs: must target a for loop" trm_for_inv_instrs t in
      let tl = Mlist.to_list tl in
      let indices =
        match indices with
        | Some indices -> indices
        | None ->
            if tl = [] then [] (* do nothing *)
            else Xlist.drop 1 (List.mapi (fun i _ -> i) tl) (* take all indices of intervals *)
          in
      fission_all_instrs_on indices t
      in
    Target.apply_at_target_paths aux tg)

(* TODO: combi
let%transfo fission_multi (tg : target) : unit =
  paths = resolve target
  all paths must be target-between inside for-loop sequences
  partition the paths according to the sequence they are in (remove duplicates)
    -> see pattern in fusion_targets ; use a group_by to generalize to multiple set of targets
    -> beware of nesting, should probably start with innermost paths
  for each for loop, apply fission on that loop, at the selected indices *)

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
  ((_index_a, start_a, dir_a, stop_a, step_a, is_par_a) : loop_range)
  ((_index_b, start_b, dir_b, stop_b, step_b, is_par_b) : loop_range) : bool =
  Internal.same_trm start_a start_b &&
  (dir_a = dir_b) &&
  Internal.same_trm stop_a stop_b &&
  same_loop_step step_a step_b &&
  (is_par_a = is_par_b)

let loop_index ((idx, _, _, _, _, _) : loop_range) : var = idx

let same_loop_index (a : loop_range) (b : loop_range) : bool =
  let ia = loop_index a in
  let ib = loop_index b in
  assert (ia.qualifier = [] && ib.qualifier = []);
  ia.name = ib.name

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
  | [(loop_range1, loop_instrs1, _contract1); (loop_range2, loop_instrs2, _contract2)] ->
    (* DEPRECATED: need to rename index anyway since #var-id
    if not (same_loop_index loop_range1 loop_range2) then
      fail t.loc "Loop_basic.fusion_on: expected matching loop indices"; *)
    if not (same_loop_range loop_range1 loop_range2) then
      fail t.loc "Loop_basic.fusion_on: expected matching loop ranges";
    let new_loop_range, _, _ = List.nth loops_ri target_loop_i in
    let (idx1, _, _, _, _, _) = loop_range1 in
    let (idx2, _, _, _, _, _) = loop_range2 in
    let loop_instrs1', loop_instrs2' =
      if upwards
      then loop_instrs1, Mlist.map (trm_subst_var idx2 (trm_var idx1)) loop_instrs2
      else Mlist.map (trm_subst_var idx1 (trm_var idx2)) loop_instrs1, loop_instrs2
    in
    let new_loop_instrs = Mlist.merge loop_instrs1' loop_instrs2' in
    (* TODO: trm_for_update on loop1? *)
    let new_loop = trm_for_instrs ~annot:lt.annot ?loc:lt.loc new_loop_range new_loop_instrs in
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
  Target.apply (fun t p ->
    let (index, p_seq) = Path.index_in_seq p in
    Target.apply_on_path (fusion_on index upwards) t p_seq
    ) tg

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
let%transfo unroll ?(inner_braces : bool = false) ?(outer_seq_with_mark : mark  = "") ?(subst_mark : mark option)  (tg : target): unit =
  Trace.justif_always_correct ();
  Nobrace_transfo.remove_after (fun _ ->
    apply_on_targets (Loop_core.unroll inner_braces outer_seq_with_mark subst_mark) tg)

(* [move_out_on trm_index t]: moves an invariant instruction just before loop [t],
    [trm_index] - index of that instruction on its surrouding sequence,
    [t] - ast of the for loop. *)
let move_out_on (mark : mark option) (trm_index : int) (t : trm) : trm =
  let tl = try for_loop_body_trms t with | TransfoError _ -> fail t.loc "Loop_basic.move_out_on: expected a for loop" in
  let lfront, trm_inv, lback = Mlist.get_item_and_its_relatives trm_index tl in
  let new_tl = Mlist.merge lfront lback in
  let loop =
  match t.desc with
  | Trm_for (l_range, _, contract) ->
    trm_for ?contract l_range (trm_seq new_tl)
  | Trm_for_c (init, cond, step, _, invariant) ->
    trm_for_c ?invariant init cond step (trm_seq new_tl)
  | _ -> fail t.loc "Loop_basic.move_out_on: expected a loop" in
  trm_seq_nobrace_nomarks [trm_may_add_mark mark trm_inv; loop]

(** [move_out tg]: expects the target [tg] to point at an instruction inside the loop
    that is not dependent on the index of the loop or any local variable.
    Then it will move it outside the loop.

    // i uninit
    for (j) {
      i = 3;
      f(i)
    }
    // i uninit
    =>
    i = 3;
    for (j) f(i);
    // ok even if loop is never executed

    On this one we will crash:
    i = 5;
    for (j) {
      i = 3;
      f(i);
    }
    =>
    i = 5
    if (range non empty) i = 3; // this would be needed for correctness but is not generated
    for (j) f(i);
*)
let%transfo move_out ?(mark : mark option) (tg : target) : unit =
  Resources.required_for_check ();
  Nobrace_transfo.remove_after ( fun _ ->
  apply_on_transformed_targets (Path.index_in_surrounding_loop) (fun t (i, p) ->
    if !Flags.check_validity then begin
      assert (i = 0);
      let loop = Path.resolve_path p t in
      let error = "Loop_basic.move_out: expected for loop" in
      let ((index, _, _, _, _, _), body, _) = trm_inv ~error trm_for_inv loop in
      let instrs = trm_inv ~error trm_seq_inv body in
      let (instr, rest) = Xlist.uncons (Mlist.to_list instrs) in
      if Var_set.mem index (trm_free_vars instr) then
        (* NOTE: would be checked by var ids anyway *)
        fail instr.loc "Loop_basic.move_out: instruction uses loop index";
      Resources.assert_dup_instr_redundant 0 (Mlist.length instrs - 1) body;
      Trace.justif "instructions from following iterations are redundant with first iteration"
      (* TODO: Check that we can add forget_init on i after the loop *)
    end;
    apply_on_path (move_out_on mark i) t p
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


(* [shift_on index kind]: shifts a loop index to start from zero or by a given amount. *)
let shift_on (index : string) (kind : shift_kind) (t : trm): trm =
  let index' = new_var index in
  let error = "Loop_basic.shift_on: expected a target to a simple for loop" in
  let ((index, start, direction, stop, step, is_parallel), body_terms, _contract) =
    trm_inv ~error trm_for_inv_instrs t in
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
  (* NOTE: assuming int type if no type is available *)
  let body_terms' = Mlist.push_front (
    trm_let_immut (index, (Option.value ~default:(typ_int ()) start.typ))
      (trm_sub (trm_var index') shift)) body_terms in
  let t2 = trm_for_instrs ~annot:t.annot (index', start', direction, stop', step, is_parallel) body_terms' in
  t2

(* [shift index kind]: shifts a loop index range according to [kind], using a new [index] name.
  *)
let%transfo shift ?(reparse : bool = false) (index : string) (kind : shift_kind) (tg : target) : unit =
  (* FIXME: having to think about reparse here is not great *)
  reparse_after ~reparse (
    Target.apply_at_target_paths (shift_on index kind)) tg

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
  let ((index, start, direction, stop, step, is_parallel), body, _contract) = trm_inv ~error trm_for_inv t in
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
  trm_for ~annot:t.annot (index, start', direction, stop', step, is_parallel) body''

(* [extend_range]: extends the range of a loop on [lower] and/or [upper] bounds.
   The body of the loop is guarded by ifs statements, doing nothing on the extension points.

   For this to be correct, the loop bounds must be extended, not shrinked.
  *)
let%transfo extend_range ?(start : extension_kind = ExtendNothing) ?(stop : extension_kind = ExtendNothing) (tg : target) : unit =
  Target.apply_at_target_paths (extend_range_on start stop) tg

(* [rename_index new_index]: renames the loop index variable *)
let%transfo rename_index (new_index : string) (tg : target) : unit =
  apply_on_targets (Loop_core.rename_index new_index) tg

(* FIXME: duplicated code from tiling. *)
let slide_on (tile_index : string) (bound : tile_bound) (tile_size : trm) (tile_step : trm) (t : trm) : trm =
  let error = "Loop_basic.slide_on: only simple loops are supported." in
  let ((index, start, direction, stop, step, is_parallel), body, _contract) = trm_inv ~error trm_for_inv t in
  let tile_index = new_var (Tools.string_subst "${id}" index.name tile_index) in
  let tile_bound =
   if is_step_one step then trm_add (trm_var tile_index) tile_size else trm_add (trm_var tile_index) (trm_mul tile_size (loop_step_to_trm step)) in
  let inner_loop =
  begin match bound with
  | TileBoundMin ->
    let tile_bound =
    trm_apps (trm_var (name_to_var "min")) [stop; tile_bound] in
    trm_for (index, (trm_var tile_index), direction, (tile_bound), step, is_parallel) body
  | TileDivides ->
    trm_for (index, (trm_var tile_index), direction, (tile_bound), step, is_parallel) body
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
      trm_for (tile_index, start, direction, outer_stop, outer_loop_step, is_parallel) (trm_seq_nomarks [inner_loop])
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
    Option.bind (trm_for_inv_instrs t_loop) (fun (_, body, _) ->
      if Mlist.is_empty body
      (* 2. delete *)
      then Some (Sequence_core.delete i 1 t_seq [])
      else None
    ))

(* [delete_void]: deletes a loop with empty body. *)
let%transfo delete_void (tg : target) : unit =
  Trace.justif_always_correct ();
  Target.apply (fun t p ->
    let (i, p_seq) = Path.index_in_seq p in
    Path.apply_on_path (fun t_seq ->
      match delete_void_on i t_seq with
      | Some t2 -> t2
      | None ->
        let loop_t = Path.get_trm_at_path p t in
        fail loop_t.loc "Loop_basic.delete_void: expected a simple loop with empty body, surrounded by a sequence"
    ) t p_seq
  ) tg
