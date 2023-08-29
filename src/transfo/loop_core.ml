open Syntax
open Target

(*  [color_aux nb_colors i_color t]: transform a loop into two nested loops based on the coloring pattern,
      [nb_colors] - a variable used to represent the number of colors,
      [i_color] - a variable representing the index used of the new outer loop,
      [t] - ast of the loop. *)
let color_aux (nb_colors : trm) (i_color : string option) (t : trm) : trm =
  let error = "Loop_core.color_aux: only simple loops are supported." in
  let ((index , start, direction, stop, step, is_parallel), body) = trm_inv ~error trm_for_inv t in
  let i_color = new_var (match i_color with
   | Some cl -> cl
   | _ -> "c" ^ index.name
  ) in
  let is_step_one =
    begin match step with
    | Post_inc | Pre_inc -> true
    | _ -> false
    end in
  let nb_colors = nb_colors in
    trm_pass_labels t (trm_for (i_color, start, direction, nb_colors, (Post_inc), is_parallel) (
      trm_seq_nomarks [
        trm_for (index, (if is_step_one then trm_var i_color else trm_apps (trm_binop Binop_mul) [trm_var i_color; loop_step_to_trm step]), direction, stop,
          (if is_step_one then Step nb_colors else Step (trm_apps (trm_binop Binop_mul) [nb_colors; loop_step_to_trm step])), is_parallel) body
      ]
  ))

(* [color nb_colors i_color t p]: applies [color_aux] at trm [t] with path [p] *)
let color (nb_colors : trm) (i_color : string option ) : Transfo.local =
    apply_on_path (color_aux nb_colors  i_color)

(*  [tile_aux divides b tile_index t]: tiles loop [t],
      [tile_index] - string representing the index used for the new outer loop,
      [bound] - a tile_bound type variable representing the type of the bound used in
                 this transformation,
      [t] - ast of targeted loop. *)
let tile_aux (tile_index : string) (bound : tile_bound) (tile_size : trm) (t : trm) : trm =
  let error = "Loop_core.tile_aux: only simple loops are supported." in
  let ((index, start, direction, stop, step, is_parallel), body) = trm_inv ~error trm_for_inv t in
  let tile_index = new_var (Tools.string_subst "${id}" index.name tile_index) in
  (* TODO: enable other styles for TileDivides *)
  if bound = TileDivides then begin
     (* TODO: other cases *)
     assert (Internal.same_trm start (trm_int 0));
     assert (direction = DirUp);
     let (count, iteration_to_index) =
      if is_step_one step
        then (stop, fun i -> i)
        else match step with
        | Step s ->
          (trm_div stop s, fun i -> trm_mul i s)
        | _ -> assert false
       in
     let ratio : int option =
       match trm_int_inv count, trm_int_inv tile_size with
       | Some ncount, Some ntile_size
           when ntile_size > 0 && ncount mod ntile_size = 0 -> Some (ncount / ntile_size)
       | _ -> None
       in
     if !Flags.check_validity then begin
       if ratio = None
         then fail t.loc "Could not syntactically check that loop bound is divisible by tile size";
       Trace.justif "loop range is syntactically dividable by tile size";
     end;
     let tile_count =
       match ratio with
       | Some r -> trm_int r
       | None -> trm_exact_div count tile_size
       in
     let iteration = trm_add (trm_mul (trm_var ?typ:start.typ tile_index) tile_size)
      (trm_var ?typ:start.typ index)
     in
     let new_index = iteration_to_index iteration in
     trm_for (tile_index, (trm_int 0), DirUp, tile_count, Post_inc, is_parallel) (trm_seq_nomarks [
       trm_for (index, (trm_int 0), DirUp, tile_size, Post_inc, is_parallel) (Internal.change_trm (trm_var index) new_index body)
     ])
  end else begin
  Trace.justif "Tiling in this form is always correct.";
  let tile_bound =
   if is_step_one step then trm_add (trm_var tile_index) tile_size else trm_add (trm_var tile_index ) (trm_mul tile_size (loop_step_to_trm step)) in
  let inner_loop =
   begin match bound with
   | TileBoundMin ->
     let tile_bound =
     trm_apps (trm_var (name_to_var "min")) [stop; tile_bound] in
     trm_for (index, (trm_var tile_index), direction, (tile_bound), step, is_parallel) body
   | TileDivides ->
     (* TODO: should be assert false ? *)
     trm_for (index, (trm_var tile_index), direction, (tile_bound), step, is_parallel) body
   | TileBoundAnd ->
     let init = trm_let_mut (index, typ_int ()) (trm_var tile_index) in
     let cond = trm_and (trm_ineq direction (trm_var_get index)
       (if is_step_one step
         then (trm_add (trm_var tile_index) tile_size)
         else (trm_add (trm_var tile_index) (trm_mul tile_size (loop_step_to_trm step) ) ))) (trm_ineq direction (trm_var_get index) stop)
      in
     let step =  if is_step_one step then trm_apps (trm_unop Unop_post_inc) [trm_var_get index]
       else trm_prim_compound Binop_add (trm_var index) (loop_step_to_trm step) in
     let new_body = trm_subst_var index (trm_var_get index) body in
     trm_for_c init cond step new_body
   end in
   let outer_loop_step = if is_step_one step then Step tile_size else Step (trm_mul tile_size (loop_step_to_trm step)) in
   let outer_loop =
      trm_for (tile_index, start, direction, stop, outer_loop_step, is_parallel) (trm_seq_nomarks [inner_loop])
   in
   trm_pass_labels t outer_loop
  end

(* [tile tile_index bound tile_size t p]: applies [tile_aux] at trm [t] with path [p] *)
let tile (tile_index : string) (bound : tile_bound) (tile_size : trm) : Transfo.local =
   apply_on_path (tile_aux tile_index bound tile_size )

(* [hoist_aux name decl_index array_size t]: extracts a variable declared inside a loop as an array of size [loop_bound -1]
    then replace all the occurrences of that variable with an array access at the loop index,
      [name] - pattern of the form "${var}_something" for the name entered by the user, if not
              entered by the user, the dafault pattern ${var}_step is used,
      [t] - ast of the loop. *)
(* LATER/ deprecated *)
let hoist_aux (name : string) (decl_index : int) (array_size : trm option) (t : trm) : trm =
  match t.desc with
  | Trm_for (l_range, body, contract) ->
    begin match body.desc with
    | Trm_seq tl ->
      let (index, _, _, stop, _, _) = l_range in
      (* TODO: stop - start ; check step *)
      (* Arith.simpl *)
      let stop_bd = begin match array_size with | Some arr_sz -> arr_sz | None -> stop end in
      let ty = ref (typ_auto()) in
      let new_name = ref dummy_var in
      let f_update (t : trm) : trm =
        match t.desc with
        | Trm_let (vk, (x, tx), _, _) ->
          new_name := new_var (Tools.string_subst "${var}" x.name name);
          ty := get_inner_ptr_type tx;
          trm_let_ref (x, (get_inner_ptr_type tx)) (trm_apps (trm_binop Binop_array_access) [trm_var_get !new_name; trm_var index] )
        | _ -> fail t.loc "Loop_core.hoist_aux: expected a variable declaration"
        in
      let new_tl = Mlist.update_nth decl_index f_update tl in
      let new_body = trm_seq new_tl in
        trm_seq_no_brace [
          trm_let_array Var_mutable (!new_name, !ty) (Trm stop_bd) (trm_uninitialized ());
          trm_for ?contract l_range new_body ]
    | _ -> fail t.loc "Loop_core.hoist_aux: body of the loop should be a sequence"
    end
  | _ -> fail t.loc "Loop_core.hoist_aux: only simple loops are supported"

(* [hoist name index array_size t p]: applies [hoist_aux] at trm [t] with path [p]. *)
(* LATER/ deprecated *)
let hoist_old (name : string) (index : int) (array_size : trm option): Transfo.local =
   apply_on_path (hoist_aux name index array_size)

(* [fusion_on_block_aux t]: merges two or more loops with the same components except the body,
      [t] - ast of the sequence containing the loops. *)
let fusion_on_block_aux (keep_label : bool) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let n = Mlist.length tl in
    if n < 2 then fail t.loc "fission_aux: there must be >= 2 loops to apply fussion";
    let first_loop = Mlist.nth tl 0 in
     begin match  first_loop.desc with
    | Trm_for (l_range, _, contract) ->
      let fusioned_body = Mlist.fold_lefti (
        fun i acc loop ->
          if not (Internal.is_trm_loop loop) then fail loop.loc (Printf.sprintf "Loop_core.fusion_on_block_aux: cannot
                                                               fuse %d loops as requested only %d where found" n (i+1))
           else
          acc @ (Mlist.to_list (for_loop_body_trms loop))
      ) [] tl in
      let res = trm_for ?contract l_range (trm_seq_nomarks fusioned_body) in
      if keep_label then trm_pass_labels t res else res
    | _ -> fail t.loc "Loop_core.fusion_on_block_aux: all loops should be simple loops"
    end
  | _ -> fail t.loc "Loop_core.fission_aux: expected a sequence of for loops"

(* [fusion_on_block keep_label t p]: applies [fusion_on_block_aux t p] at trm [t] with path [p]. *)
let fusion_on_block (keep_label : bool): Transfo.local =
  apply_on_path (fusion_on_block_aux keep_label)

(* [grid_enumerate_aux indices_and_bounds t]: transforms a loop over a grid into nested loops over
    each dimension of that grid,
      [indices_and_bounds] - a list of pairs representing the index and the bound for each dimension,
      [t] - ast of the loop. *)
let grid_enumerate_aux (indices_and_bounds : (string * trm) list) (t : trm) : trm =
  let error = "Loop_core.grid_enumerate_aux: expected a simple for loop" in
  let (l_range, body) = trm_inv ~error trm_for_inv t in
  let (index, _, direction, _, _, is_parallel) = l_range in
  let indices_and_bounds = List.map (fun (i, b) -> Trm.new_var i, b) indices_and_bounds in
  let new_body =
    begin match body.desc with
    | Trm_seq tl ->
        let old_loop_index_val = Xlist.fold_lefti (fun i acc (ind, bnd) ->
            if i = 0 then let acc = trm_var ind in acc
            else trm_apps (trm_binop Binop_add) [trm_apps (trm_binop Binop_mul) [
                acc; bnd]; trm_var ind])  (trm_unit ()) indices_and_bounds in
        let old_loop_index_decl = trm_let_immut (index, typ_int ()) old_loop_index_val in
        let new_tl = Mlist.insert_at 0 old_loop_index_decl tl in
        trm_seq new_tl
    | _ -> fail body.loc "Loop_core.grid_enumerate_aux: the body of the loop should be a sequence"
    end in

    Xlist.fold_lefti (fun i acc (ind, bnd) ->
      if i = 0 then  trm_for (ind, (trm_int 0), direction, bnd, (Post_inc), is_parallel) acc
        else  trm_for (ind, (trm_int 0), DirUp, bnd, Post_inc, false) (trm_seq_nomarks [acc])
    ) new_body (List.rev indices_and_bounds)

(* [grid_enumerate indices_and_bounds t p]: applies [grid_enumerate_aux indices_and_bounds] at trm [t] with path [p]. *)
let grid_enumerate (indices_and_bounds : (string * trm) list) : Transfo.local =
  apply_on_path (grid_enumerate_aux indices_and_bounds)

(* [unroll_aux index t]: unrolls loop [t],
      [inner_braces] - a flag on the visibility of generated inner sequences,
      [outer_seq_mark] - generates an outer sequence with a mark,
      [t] - ast of the loop. *)
let unroll_aux (inner_braces : bool) (outer_seq_with_mark : mark) (subst_mark : mark option) (t : trm) : trm =
  let error = "Loop_core.unroll_aux: only simple loops supported" in
  let (l_range, body) = trm_inv ~error trm_for_inv t in
  let (index, start, _, stop, _, _) = l_range in
  let unrolled_loop_range =
    begin match stop.desc with
    | Trm_apps(_,[_; bnd]) ->
        begin match bnd.desc with
        | Trm_val (Val_lit (Lit_int bnd)) ->
          Xlist.range 0 (bnd - 1)
        | _ -> fail bnd.loc "Loop_core.unroll_aux: expected a literal trm"
        end
      | Trm_val (Val_lit (Lit_int bnd)) ->
          begin match start.desc with
          | Trm_val (Val_lit (Lit_int strt)) ->
            Xlist.range 0 (bnd - 1 - strt)
          | _ -> fail start.loc "Loop_core.unroll_aux: expected a "
          end
    | _ -> fail t.loc "Loop_core.unroll_aux: the loop that is going to be unrolled should have a bound which is a sum of a variable and a literal"
    end in
  let unrolled_body = List.fold_left ( fun acc i1 ->
    let new_index =
      begin match start.desc with
      | Trm_val (Val_lit (Lit_int n)) -> trm_lit (Lit_int (n + i1))
      | _ -> trm_apps (trm_binop Binop_add) [start; (trm_lit (Lit_int i1))]
      end in
    let body_i = trm_subst_var index (trm_may_add_mark subst_mark new_index) (trm_copy body) in
    let body_i = if inner_braces
                  then Nobrace.remove_if_sequence body_i
                  else Nobrace.set_if_sequence body_i in
    body_i :: acc ) [] (List.rev unrolled_loop_range) in
  begin match outer_seq_with_mark with
  | "" -> trm_seq_no_brace unrolled_body
  | _ -> trm_add_mark outer_seq_with_mark (trm_seq_nomarks unrolled_body)
  end

(* [unroll braces my_mark t p]: applies [unroll_aux] at trm [t] with path [p]. *)
let unroll (inner_braces : bool) (outer_seq_with_mark : mark) (subst_mark : mark option) : Transfo.local =
  apply_on_path (unroll_aux inner_braces outer_seq_with_mark subst_mark)

(* [move_out_aux trm_index t]: moves an invariant instruction just before loop [t],
    [trm_index] - index of that instruction on its surrouding sequence,
    [t] - ast of the for loop. *)
let move_out_aux (mark : mark option) (trm_index : int) (t : trm) : trm =
  let tl = try for_loop_body_trms t with | TransfoError _ -> fail t.loc "Loop_core.move_out_aux: expected a for loop" in
  let lfront, trm_inv, lback = Mlist.get_item_and_its_relatives trm_index tl in
  let new_tl = Mlist.merge lfront lback in
  let loop =
  match t.desc with
  | Trm_for (l_range, _, contract) ->
    trm_for ?contract l_range (trm_seq new_tl)
  | Trm_for_c (init, cond, step, _, invariant) ->
    trm_for_c ?invariant init cond step (trm_seq new_tl)
  | _ -> fail t.loc "Loop_core.move_out_aux: expected a loop" in
  trm_seq_no_brace [trm_may_add_mark mark trm_inv; loop]

(* [move_out trm_index t p]: applies [move_out_aux] at trm [t] with path [p] *)
let move_out (mark : mark option) (trm_index : int) : Transfo.local =
  apply_on_path (move_out_aux mark trm_index)

(* [unswitch_aux trm_index t]: extracts an if statement inside the loop whose condition,
    is not dependent on the index of the loop or any other local variables,
      [trm_index] - index of the if statement inside the body of the loop,
      [t] - ast of the for loop. *)
let unswitch_aux (trm_index : int) (t : trm) : trm =
  let tl = for_loop_body_trms t in
  let if_stmt = Mlist.nth tl trm_index in
  let error = "Loop_core.unswitch_aux: expected an if statement."  in
  let (cond, then_, else_) = trm_inv ~error trm_if_inv if_stmt in
  let then_ = Nobrace.set_if_sequence then_ in
  let else_ = Nobrace.set_if_sequence else_ in
  let wrap_branch (t1 : trm) : trm  = Internal.change_loop_body t (trm_seq (Mlist.replace_at trm_index t1 tl )) in
  trm_if cond (wrap_branch then_) (trm_copy (wrap_branch else_))

(* [unswitch trm_index t p]: applies [unswitch_aux] at trm [t] with path [p]. *)
let unswitch (trm_index : int) : Transfo.local =
  apply_on_path (unswitch_aux trm_index)

(* [to_unit_steps_aux new_index t]: transforms loop [t] into a loop with unit steps,
      [new_index] - a string representing the new index for the transformed loop,
      [t] - ast of the loop to be transformed. *)
let to_unit_steps_aux (new_index : string) (t : trm) : trm =
  let error = "Loop_core.to_unit_steps: only simple loops are supported." in
  let ((index, start, direction, stop, step, is_parallel), _) = trm_inv ~error trm_for_inv t in
  let new_index = new_var (match new_index with
  | "" -> index.name ^ "_step"
  | _ -> new_index) in

  let body_trms = for_loop_body_trms t in
  let body_trms = Mlist.map (fun t -> Internal.change_trm (trm_var index) (trm_var_get index) t) body_trms in
  let loop_step = match step with
  | Step l_step -> l_step
  | _ -> trm_int 1 in
   let aux (start : trm) (stop : trm) : trm =
     match trm_lit_inv start with
     | Some (Lit_int 0) ->
       stop
     | _ -> trm_sub stop start
    in

  let new_stop  =
  begin match direction with
  | DirUp ->  (trm_div (aux start stop) loop_step)
  | DirUpEq -> (trm_div (aux start stop) loop_step)
  | DirDown -> (trm_div (aux start stop) loop_step)
  | DirDownEq -> (trm_div (aux start stop) loop_step)
  end in

  let new_decl = trm_let_mut (index, typ_int() ) (trm_apps (trm_binop Binop_add)[
          start;
          trm_apps (trm_binop Binop_mul) [trm_var new_index; loop_step]
        ]) in
  trm_for (new_index, (trm_int 0), direction, new_stop, Post_inc, is_parallel)
    (trm_seq (Mlist.insert_at 0 new_decl body_trms ))

(* [loop_to_unit_steps new_index t p]: applies [to_unit_steps_aux] to the trm [t] with path [p]. *)
let to_unit_steps (new_index : string) : Transfo.local =
  apply_on_path (to_unit_steps_aux new_index)


(* [fold_aux index start step t]: transforms a sequence of instructions into a for loop,
      [index] - index of the generated for loop,
      [start] - starting value for the index of the generated for loop,
      [step] - step of the generated for loop,
      [t] - ast of the sequence.

    NOTE: we trust the user that "stop" corresponds to the number of iterations
    LATER: use  sExpr  to mark the subexpression that correspnod to the string "start";
    then you can Generic.replace at these marks.*)
let fold_aux (index : string) (start : int) (step : int) (t : trm) : trm =
  let index = new_var index in
  let error = "Loop_core.fold_aux: expected a sequence of instructions" in
  let tl = trm_inv ~error trm_seq_inv t in
  let nb = Mlist.length tl in
  if nb = 0
    then fail t.loc "Loop_core.fold_aux: expected a non-empty list of instructions";
  let first_instr, other_instr  = Xlist.uncons (Mlist.to_list tl) in
  let loop_body = Internal.change_trm (trm_int start) (trm_var index) first_instr in
  List.iteri( fun i t1 ->
    let local_body = Internal.change_trm (trm_int (i+1)) (trm_var index) t1 in
    if not (Internal.same_trm loop_body local_body)
      then fail t1.loc "Loop_core.fold_aux: all the instructions should have the same shape but differ by the index";
  ) other_instr;
  trm_pass_labels t (trm_for (index, (trm_int start), DirUp, (trm_int nb), (if step = 1 then Post_inc else Step (trm_int step)), false) (trm_seq_nomarks [loop_body]))

(* [fold index start step t p]: applies [fold_aux] at trm [t] with path [p]. *)
let fold (index : string) (start : int) (step : int) : Transfo.local =
  apply_on_path (fold_aux index start step)

(* [split_range_aux nb cut]: splits a loop into two loops based on the range,
     [nb] - by default this argument has value 0, if provided it means that it will split the loop at start + nb iteration,
     [cut] - by default this argument has value tmr_unit(), if provided then the loop will be splited at that iteration,
     [t] - ast of the for loop. *)
let split_range_aux (nb : int)(cut : trm)(t : trm) : trm =
  let error = "Loop_core.split_range: expected a target to a simple for loop" in
  let ((index, start, direction, stop, step, is_parallel), body) = trm_inv ~error trm_for_inv t in
  let split_index =
  begin match nb, cut with
  | 0, {desc = Trm_val (Val_lit (Lit_unit )); _} -> fail t.loc "Loop_core.split_range_aux: one of the args nb or cut should be set "
  | 0, _ -> cut
  | _, {desc = Trm_val (Val_lit (Lit_unit ));_} -> trm_add (trm_var index) (trm_lit (Lit_int nb))
  | n, c -> fail t.loc "Loop_core.split_range_aux: can't provide both the nb and cut args"
  end in
  trm_seq_no_brace [
    trm_for (index, start, direction, split_index, step, is_parallel) body;
    trm_copy (trm_for (index, split_index, direction, stop, step, is_parallel) body)]

(* [split_range nb cut t p]: applies [split_range_aux] at the trm [t] with path [p]. *)
let split_range (nb : int) (cut : trm) : Transfo.local =
  apply_on_path (split_range_aux nb cut)

(* [rename_index new_index]: renames the loop index variable *)
let rename_index (new_index : string) : Transfo.local =
  apply_on_path (fun t ->
    let error = "Loop_core.shift: expected a target to a simple for loop" in
    let ((index, start, direction, stop, step, is_parallel), body) = trm_inv ~error trm_for_inv t in
    let new_index = { qualifier = []; name = new_index; id = index.id } in
    let new_body = trm_subst_var index (trm_var new_index) body in
    trm_for ~annot:t.annot (new_index, start, direction, stop, step, is_parallel) new_body
  )
