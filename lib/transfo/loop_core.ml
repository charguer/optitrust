open Prelude
open Target

(** [color_on nb_colors i_color t]: transform a loop into two nested loops based on the coloring pattern,
      [nb_colors] - a variable used to represent the number of colors,
      [i_color] - a variable representing the index used of the new outer loop,
      [t] - ast of the loop.

      todo check start is zero *)
let color_on (nb_colors : trm) (i_color : string option) (t : trm) : trm =
  let error = "Loop_core.color_aux: only simple loops are supported." in
  let ({index; start; direction; stop; step}, body, _contract) = trm_inv ~error trm_for_inv t in
  let i_color = new_var (match i_color with
   | Some cl -> cl
   | _ -> "c" ^ index.name
  ) in
  let is_step_one = trm_is_one step in
  (* FIXME: Loop bounds are at least unintuitive and probably broken... *)
  let nb_colors = nb_colors in
    trm_pass_labels t (trm_for { index = i_color; start; direction; stop = nb_colors; step = trm_step_one () } (
      trm_seq_nomarks [
        trm_for {
          index;
          start = (if is_step_one then trm_var i_color else trm_mul ~typ:typ_isize (trm_var i_color) step); (* utiliser un combinateur trm_mul_smart *)
          direction;
          stop;
          step = (if is_step_one then nb_colors else (trm_mul ~typ:typ_isize nb_colors step)) } body
      ]
  ))

(** [tile_bound]: used for loop tiling transformation *)
type tile_bound = TileBoundMin | TileBoundAnd | TileDivides

let tile_bound_to_string = function
  | TileBoundMin -> "TileBoundMin"
  | TileBoundAnd -> "TileBoundAnd"
  | TileDivides -> "TileDivides"

let ghost_tile_divides = toplevel_var "tile_divides"
let ghost_untile_divides = toplevel_var "untile_divides"

let ghost_ro_tile_divides = toplevel_var "ro_tile_divides"
let ghost_ro_untile_divides = toplevel_var "ro_untile_divides"

let ghost_tiled_index_in_range = toplevel_var "tiled_index_in_range"

(** [tile_on divides b tile_index t]: tiles loop [t],
      [tile_index] - string representing the index used for the new outer loop,
      [bound] - a tile_bound type variable representing the type of the bound used in
                 this transformation,
      [t] - ast of targeted loop. *)
let tile_on (tile_index : string) (bound : tile_bound) (tile_size : trm) (t : trm) : trm =
  let error = "Loop_core.tile_aux: only simple loops are supported." in
  let ({index; start; direction; stop; step}, body, contract) = trm_inv ~error trm_for_inv t in
  let tile_index = new_var (Tools.string_subst "${id}" index.name tile_index) in
  (* TODO: enable other styles for TileDivides *)
  if bound = TileDivides then begin
    (* TODO: other cases *)
    assert (are_same_trm start (trm_int 0));
    assert (direction = DirUp);
    let (count, iteration_to_index) =
      if trm_is_one step
        then (stop, fun i -> i)
        else (trm_trunc_div_int stop step, fun i -> trm_mul_int i step)
    in
    let ratio : int option =
      match trm_int_inv count, trm_int_inv tile_size with
      | Some ncount, Some ntile_size
          when ntile_size > 0 && ncount mod ntile_size = 0 -> Some (ncount / ntile_size)
      | _ -> None
      in
    let tile_count =
      match ratio with
      | Some r -> trm_int r
      | None -> trm_exact_div_int count tile_size
      in

    let div_check_var = new_var ("tile_div_check_" ^ index.name) in
    let div_check_assert = Resource_trm.ghost_assert div_check_var Resource_formula.(formula_eq ~typ:typ_int count (trm_mul_int tile_count tile_size)) in
    let div_check = trm_var div_check_var in

    let iteration = trm_add_int (trm_mul_int (trm_var tile_index) tile_size) (trm_var index)
    in
    let new_index = iteration_to_index iteration in
    let outer_range = { index = tile_index; start = (trm_int 0); direction = DirUp; stop = tile_count; step = trm_step_one () } in
    let inner_range = { index; start = (trm_int 0); direction = DirUp; stop = tile_size; step = trm_step_one () } in

    if not contract.strict then begin
      if !Flags.check_validity then begin
        Trace.justif "loop range is checked to be dividable by tile size";
        trm_seq_nobrace_nomarks [
          div_check_assert;
          trm_for outer_range (trm_seq_nomarks [
            trm_for inner_range (trm_subst_var index new_index body)
          ])
        ]
      end else
        trm_for outer_range (trm_seq_nomarks [
          trm_for inner_range (trm_subst_var index new_index body)
        ])
    end else
      let open Resource_formula in

      let contract_inner = {
        loop_ghosts = contract.loop_ghosts;
        invariant = Resource_set.subst_var index new_index contract.invariant;
        parallel_reads = contract.parallel_reads;
        iter_contract = {
          pre = Resource_set.subst_var index new_index contract.iter_contract.pre;
          post = Resource_set.subst_var index new_index contract.iter_contract.post };
        strict = true } in

      let outer_index = iteration_to_index (trm_mul_int (trm_var tile_index) tile_size) in

      let contract_outer = {
        loop_ghosts = contract.loop_ghosts;
        invariant = Resource_set.subst_var index outer_index contract.invariant;
        parallel_reads = contract.parallel_reads;
        iter_contract = {
          pre = Resource_set.group_range inner_range contract_inner.iter_contract.pre;
          post = Resource_set.group_range inner_range contract_inner.iter_contract.post };
        strict = true } in

      (* TODO: Also tile groups in pure resources *)
      (* TODO: Give the used resource instead of specifying ghost parameters? *)
      let tiling_ghosts ghost ro_ghost =
        List.map (fun (_, formula) ->
            let ghost, formula =
              match formula_read_only_inv formula with
              | Some { formula } -> ro_ghost, formula
              | None -> ghost, formula
            in
            let i = new_var index.name in
            let items = formula_fun [i, typ_int] (trm_subst_var index (trm_var i) formula) in
            Resource_trm.ghost (ghost_call ghost [("div_check", div_check); ("items", items)])
          )
      in
      let ghosts_before = tiling_ghosts ghost_tile_divides ghost_ro_tile_divides contract.iter_contract.pre.linear in
      let ghosts_after = tiling_ghosts ghost_untile_divides ghost_ro_untile_divides contract.iter_contract.post.linear in

      let open Resource_trm in
      let indexed_invariant = Resource_set.filter_with_var index contract.invariant in
      let rewrite_indexed_invariant_ghosts rew_eq =
        rewrite_var_in_res_ghosts ~filter_changed:false index ~by:rew_eq indexed_invariant
      in
      let rew_ghosts_before_outer = rewrite_indexed_invariant_ghosts (trm_apps logic_zero_mul_intro [tile_size]) in
      let rew_ghosts_before_inner = rewrite_indexed_invariant_ghosts
        (trm_apps logic_plus_zero_intro [outer_index]) in
      let rew_ghost_end_inner = rewrite_indexed_invariant_ghosts (trm_apps logic_add_assoc_right [trm_mul_int (trm_var tile_index) tile_size; trm_var index; (trm_int 1)]) in
      let rew_ghosts_after_inner = rewrite_indexed_invariant_ghosts (trm_apps logic_mul_add_factor [trm_var tile_index; tile_size]) in
      let rew_ghosts_after_outer = rewrite_indexed_invariant_ghosts (trm_apps logic_eq_sym [count; trm_mul_int tile_count tile_size; trm_var div_check_var]) in

      let body_seq, _ = trm_inv trm_seq_inv (trm_subst_var index new_index body) in
      let ghost_tile_index = Resource_trm.ghost (ghost_call ghost_tiled_index_in_range [("tile_index", trm_var tile_index); ("index", trm_var index); ("div_check", div_check)]) in

      let body = trm_like ~old:body (trm_seq_helper [Trm ghost_tile_index; TrmMlist body_seq; TrmList rew_ghost_end_inner]) in

      Trace.justif "loop range is checked to be dividable by tile size";
      trm_seq_nobrace_nomarks (
        div_check_assert ::
        ghosts_before @
        rew_ghosts_before_outer @ [
        trm_for ~contract:contract_outer outer_range (trm_seq_nomarks (rew_ghosts_before_inner @ [
          trm_copy (trm_for ~contract:contract_inner inner_range body)
        ] @ rew_ghosts_after_inner))
      ] @ rew_ghosts_after_outer @ ghosts_after)


  end else begin
    let inner_loop = match bound with
      | TileBoundMin ->
        let tile_bound =
          if trm_is_one step then trm_add_int (trm_var tile_index) tile_size else trm_add_int (trm_var tile_index) (trm_mul_int tile_size step) in
        let tile_bound =
          trm_apps (trm_var (name_to_var "min")) [stop; tile_bound] in
        trm_for { index; start = trm_var tile_index; direction; stop = tile_bound; step } body
      | TileBoundAnd ->
        let init = trm_let_mut (index, typ_int) (trm_var tile_index) in
        let cond = trm_and (trm_ineq direction (trm_var_get index)
          (if trm_is_one step
            then (trm_add_int (trm_var tile_index) tile_size)
            else (trm_add_int (trm_var tile_index) (trm_mul_int tile_size step)))) (trm_ineq direction (trm_var_get index) stop)
          in
        let step =  if trm_is_one step then trm_post_incr (trm_var index)
          else trm_compound_assign ~typ:typ_int Binop_add (trm_var index) step in
        let new_body = trm_subst_var index (trm_var_get index) body in
        trm_for_c init cond step new_body
      | TileDivides -> assert false
    in
    let outer_loop_step = if trm_is_one step then tile_size else (trm_mul_int tile_size step) in
    let outer_loop =
        trm_for { index = tile_index; start; direction; stop; step = outer_loop_step } (trm_seq_nomarks [inner_loop])
    in
    trm_pass_labels t outer_loop
  end


(** [fusion_on_block_on t]: merges two or more loops with the same components except the body,
      [t] - ast of the sequence containing the loops. *)
let fusion_on_block_on (keep_label : bool) (t : trm) : trm =
  match t.desc with
  | Trm_seq (tl, None) ->
    let n = Mlist.length tl in
    if n < 2 then trm_fail t "fusion_on_block_on: there must be >= 2 loops to apply fussion";
    let first_loop = Mlist.nth tl 0 in
    begin match first_loop.desc with
    | Trm_for (l_range, _, contract) ->
      let fusioned_body = Mlist.fold_lefti (
        fun i acc loop ->
          if not (Internal.is_trm_loop loop) then trm_fail loop (Printf.sprintf "Loop_core.fusion_on_block_aux: cannot
                                                               fuse %d loops as requested only %d where found" n (i+1))
           else
          acc @ (Mlist.to_list (for_loop_body_trms loop))
      ) [] tl in
      let res = trm_for ~contract l_range (trm_seq_nomarks fusioned_body) in
      if keep_label then trm_pass_labels t res else res
    | _ -> trm_fail t "Loop_core.fusion_on_block_on: all loops should be simple loops"
    end
  | _ -> trm_fail t "Loop_core.fusion_on_block_on: expected a sequence of for loops"

(** [grid_enumerate_on indices_and_bounds t]: transforms a loop over a grid into nested loops over
    each dimension of that grid,
      [indices_and_bounds] - a list of pairs representing the index and the bound for each dimension,
      [t] - ast of the loop. *)
let grid_enumerate_on (indices_and_bounds : (string * trm) list) (t : trm) : trm =
  let error = "Loop_core.grid_enumerate_on: expected a simple for loop" in
  let (range, body, _contract) = trm_inv ~error trm_for_inv t in
  let indices_and_bounds = List.map (fun (i, b) -> new_var i, b) indices_and_bounds in
  let new_body =
    begin match body.desc with
    | Trm_seq (tl, None) ->
        let old_loop_index_val = List.fold_lefti (fun i acc (ind, bnd) ->
            if i = 0 then let acc = trm_var ind in acc
            else trm_add_int (trm_mul_int acc bnd) (trm_var ind)
          ) (trm_unit ()) indices_and_bounds in
        let old_loop_index_decl = trm_let (range.index, typ_int) old_loop_index_val in
        let new_tl = Mlist.insert_at 0 old_loop_index_decl tl in
        trm_seq new_tl
    | _ -> trm_fail body "Loop_core.grid_enumerate_on: the body of the loop should be a sequence"
    end in

    List.fold_lefti (fun i acc (index, stop) ->
      if i = 0
        then trm_for { index; start = trm_int 0; direction = range.direction; stop; step = trm_step_one () } acc
        else trm_for { index; start = trm_int 0; direction = DirUp; stop; step = trm_step_one () } (trm_seq_nomarks [acc])
    ) new_body (List.rev indices_and_bounds)

let trm_unroll = trm_var (toplevel_var "unroll")
let trm_roll = trm_var (toplevel_var "roll")

let unroll_ghost_pair (range : loop_range) (contract : loop_contract)
  (new_indices : trm list) : (trm list * trm list) =
  let open Resource_formula in
  (* LATER: Handle formulas that depend on a model abstracted by the loop *)
  let unroll_ghosts = List.map (fun (_, formula) ->
    let output = List.map (fun new_index ->
      (new_anon_hyp (), trm_subst_var range.index new_index (trm_copy formula)))
      new_indices
    in
    let rewrite_contract = {
      pre = Resource_set.make ~linear:[new_anon_hyp (), formula_group_range range formula] () ;
      post = Resource_set.make ~linear:output ()
    } in
    (* LATER: build proof term instead *)
    Resource_trm.ghost_admitted rewrite_contract ~justif:trm_unroll
  ) contract.iter_contract.pre.linear
  in
  let unroll_pure_ghosts = List.map (fun (_, formula) ->
    let output = List.map (fun new_index ->
      (new_anon_hyp (), trm_subst_var range.index new_index (trm_copy formula)))
      new_indices
    in
    let rewrite_contract = {
      pre = Resource_set.make ~pure:[new_anon_hyp (), formula_forall_range range formula] () ;
      post = Resource_set.make ~pure:output ()
    } in
    (* LATER: build proof term instead *)
    Resource_trm.ghost_admitted rewrite_contract ~justif:trm_unroll
  ) contract.iter_contract.pre.pure
  in
  let roll_ghosts = List.map (fun (_, formula) ->
    let input = List.map (fun new_index ->
      (new_anon_hyp (), trm_subst_var range.index new_index (trm_copy formula)))
      new_indices
    in
    let rewrite_contract = {
      pre = Resource_set.make ~linear:input ();
      post = Resource_set.make ~linear:[new_anon_hyp (), formula_group_range range formula] ()
    } in
    Resource_trm.ghost_admitted rewrite_contract ~justif:trm_roll
  ) contract.iter_contract.post.linear
  in
  let roll_pure_ghosts = List.map (fun (_, formula) ->
    let input = List.map (fun new_index ->
      (new_anon_hyp (), trm_subst_var range.index new_index (trm_copy formula)))
      new_indices
    in
    let rewrite_contract = {
      pre = Resource_set.make ~pure:input ();
      post = Resource_set.make ~pure:[new_anon_hyp (), formula_forall_range range formula] ();
    } in
    (* LATER: build proof term instead *)
    Resource_trm.ghost_admitted rewrite_contract ~justif:trm_roll
  ) contract.iter_contract.post.pure
  in
  (unroll_pure_ghosts @ unroll_ghosts, roll_pure_ghosts @ roll_ghosts)

(** [unroll_on index t]: unrolls loop [t],
      [inner_braces] - a flag on the visibility of generated inner sequences,
      [outer_seq_mark] - generates an outer sequence with a mark,
      [t] - ast of the loop. *)
let unroll_on (inner_braces : bool) (outer_seq_with_mark : mark) (subst_mark : mark) (t : trm) : trm =
  let error = "Loop_core.unroll_on: only simple loops supported" in
  let (range, body, contract) = trm_inv ~error trm_for_inv t in
  let { index; start; direction; stop; step } = range in
  if direction <> DirUp then trm_fail t "Loop_core.unroll_on: only loops going upwards are supported";
  let step = trm_inv trm_int_inv step in
  let error = "Loop_core.unroll_on: either the loop start bound should be a integer literal or the loop end should be of the form 'start + k'" in
  let nb_iter =
    Pattern.pattern_match stop [
      Pattern.(trm_add !__ (trm_int !__)) (fun start' max_incr () ->
        if are_same_trm start start' then (max_incr + step - 1) / step
        else trm_fail t error
      );
      Pattern.(trm_int !__) (fun stop () ->
        let start = trm_inv ~error trm_int_inv start in
        (stop - start + step - 1) / step
      );
      Pattern.__ (fun () -> trm_fail t error)
    ]
  in
  let index_trm = match trm_int_inv start with
    | Some n -> (fun i -> trm_add_mark subst_mark (trm_int (n + i * step)))
    | None -> (fun i -> trm_add_mark subst_mark (trm_add_int start (trm_int (i * step))))
  in
  let new_indices = List.init nb_iter index_trm in

  let indexed_invariant = Resource_set.filter_with_var index contract.invariant in
  let rewrite_indexed_invariant_ghosts from into =
    Resource_trm.rewrite_var_in_res_ghosts ~filter_changed:false index ~from ~into indexed_invariant
  in
  let unrolled_body = List.mapi (fun i new_index ->
    let body_i = trm_subst_var index new_index (trm_copy body) in
    let body_i = Nobrace.set_mark (not inner_braces) body_i in
    let rewrite_ghosts = rewrite_indexed_invariant_ghosts (trm_add_int new_index (trm_int step)) (index_trm (i+1)) in
    body_i :: rewrite_ghosts) new_indices
  in
  let unrolled_body = List.concat unrolled_body in
  let outer_seq = if outer_seq_with_mark <> no_mark then
      trm_add_mark outer_seq_with_mark (trm_seq_nomarks unrolled_body)
    else
      trm_seq_nobrace_nomarks unrolled_body
  in
  if not contract.strict then
    outer_seq
  else
    let (unroll_ghosts, roll_ghosts) = unroll_ghost_pair range contract new_indices in
    trm_seq_nobrace_nomarks (unroll_ghosts @ outer_seq :: roll_ghosts)


(** [unswitch_at trm_index t]: extracts an if statement inside the loop whose condition,
    is not dependent on the index of the loop or any other local variables,
      [trm_index] - index of the if statement inside the body of the loop,
      [t] - ast of the for loop. *)
let unswitch_at (trm_index : int) (t : trm) : trm =
  let tl = for_loop_body_trms t in
  let if_stmt = Mlist.nth tl trm_index in
  let error = "Loop_core.unswitch_aux: expected an if statement."  in
  let (cond, then_, else_) = trm_inv ~error trm_if_inv if_stmt in
  let then_ = Nobrace.mark then_ in
  let else_ = Nobrace.mark else_ in
  let wrap_branch (t1 : trm) : trm  = Internal.change_loop_body t (trm_seq (Mlist.replace_at trm_index t1 tl )) in
  trm_if cond (wrap_branch then_) (trm_copy (wrap_branch else_))

(** [to_unit_steps_on new_index t]: transforms loop [t] into a loop with unit steps,
      [new_index] - a string representing the new index for the transformed loop,
      [t] - ast of the loop to be transformed. *)
let to_unit_steps_on (new_index : string) (t : trm) : trm =
  let error = "Loop_core.to_unit_steps: only simple loops are supported." in
  let ({ index; start; direction; stop; step }, _, _) = trm_inv ~error trm_for_inv t in
  let new_index = new_var (match new_index with
  | "" -> index.name ^ "_step"
  | _ -> new_index) in

  let body_trms = for_loop_body_trms t in
  let body_trms = Mlist.map (fun t -> Internal.change_trm (trm_var index) (trm_var_get index) t) body_trms in
  let aux (start : trm) (stop : trm) : trm =
    match trm_lit_inv start with
    | Some (Lit_int (_, 0)) ->
      stop
    | _ -> trm_sub_int stop start
  in

  let new_stop = match direction with
    | DirUp -> (trm_trunc_div_int (aux start stop) step)
    | DirUpEq -> (trm_trunc_div_int (aux start stop) step)
    | DirDown -> (trm_trunc_div_int (aux start stop) step)
    | DirDownEq -> (trm_trunc_div_int (aux start stop) step)
  in
  (* TODO: this should be an immutable binding *)
  let new_decl = trm_let_mut (index, typ_int) (trm_add_int start (trm_mul_int (trm_var new_index) step)) in
  trm_for { index = new_index; start = trm_int 0; direction; stop = new_stop; step = trm_step_one () }
    (trm_seq (Mlist.insert_at 0 new_decl body_trms ))

(** [fold_at index start step t]: transforms a sequence of instructions into a for loop,
      [index] - index of the generated for loop,
      [start] - starting value for the index of the generated for loop,
      [step] - step of the generated for loop,
      [t] - ast of the sequence.

    NOTE: we trust the user that "stop" corresponds to the number of iterations
    LATER: use  sExpr  to mark the subexpression that correspnod to the string "start";
    then you can Generic.replace at these marks.*)
let fold_at (index : string) (start : int) (step : int) (t : trm) : trm =
  let index = new_var index in
  let error = "Loop_core.fold_at: expected a sequence of instructions" in
  let tl, result = trm_inv ~error trm_seq_inv t in
  if Option.is_some result then failwith "Loop_core.fold_at: expected a sequence without result value";
  let nb = Mlist.length tl in
  if nb = 0
    then trm_fail t "Loop_core.fold_aux: expected a non-empty list of instructions";
  let first_instr, other_instr  = List.uncons (Mlist.to_list tl) in
  let loop_body = Internal.change_trm (trm_int start) (trm_var index) first_instr in
  List.iteri( fun i t1 ->
    let local_body = Internal.change_trm (trm_int (i+1)) (trm_var index) t1 in
    if not (are_same_trm loop_body local_body)
      then trm_fail t1 "Loop_core.fold_aux: all the instructions should have the same shape but differ by the index";
  ) other_instr;
  trm_pass_labels t (trm_for { index; start = trm_int start; direction = DirUp; stop = trm_int nb; step = (if step = 1 then trm_step_one () else (trm_int step)) } (trm_seq_nomarks [loop_body]))


let ghost_group_split = toplevel_var "group_split"
let ghost_ro_group_split = toplevel_var "ro_group_split"
let ghost_pure_group_split = toplevel_var "pure_group_split"
let ghost_group_join = toplevel_var "group_join"
let ghost_ro_group_join = toplevel_var "ro_group_join"
let ghost_pure_group_join = toplevel_var "pure_group_join"

(** [split_range_at nb cut]: splits a loop into two loops based on the range,
     [nb] - by default this argument has value 0, if provided it means that it will split the loop at start + nb iteration,
     [cut] - by default this argument has value tmr_unit(), if provided then the loop will be splited at that iteration,
     [t] - ast of the for loop.

     TODO: Optional arguments instead of weird "default" values
     *)
let split_range_at (nb : int) (cut : trm) (t : trm) : trm =
  let error = "Loop_core.split_range: expected a target to a simple for loop" in
  let (range, body, _contract) = trm_inv ~error trm_for_inv t in
  let split_index = match nb, cut with
    | 0, {desc = Trm_lit (Lit_unit); _} -> trm_fail t "Loop_core.split_range_aux: one of the args nb or cut should be set "
    | 0, _ -> cut
    | _, {desc = Trm_lit (Lit_unit);_} ->
      begin match trm_int_inv range.start with
      | Some start -> trm_int (start + nb)
      | None -> trm_add_int range.start (trm_int nb)
      end
    | n, c -> trm_fail t "Loop_core.split_range_aux: can't provide both the nb and cut args"
  in
  trm_seq_nobrace_nomarks [
    trm_for { range with stop = split_index } body;
    trm_copy (trm_for { range with start = split_index } body)]

(** [rename_index_on new_index]: renames the loop index variable *)
let rename_index_on (new_index : string) (t: trm) : trm =
  let error = "Loop_core.shift: expected a target to a simple for loop" in
  let (range, body, contract) = trm_inv ~error trm_for_inv t in
  let new_index = { namespaces = []; name = new_index; id = range.index.id } in
  let new_body = trm_subst_var range.index (trm_var new_index) body in
  let new_contract = Resource_contract.loop_contract_subst (Var_map.singleton range.index (trm_var new_index)) contract in
  trm_for ~annot:t.annot ~contract:new_contract { range with index = new_index } new_body
