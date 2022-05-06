open Ast
open Target

(* [swap_aux t]: swaps the order of two nested loops, the targeted loop with the immediate inner loop,
       [t] - ast of the targeted loop. *)
let swap_aux (t : trm) : trm =
  match Internal.extract_loop t with
  | Some (loop1, body1) ->
    begin match body1.desc with
    | Trm_seq tl when Mlist.length tl = 1 ->
      let loop2 = Mlist.nth tl 0 in
      begin match Internal.extract_loop loop2 with
      | Some (loop2, body2) -> loop2 (trm_seq_nomarks [loop1 body2])
      | None -> fail body1.loc "Loop_core..swap_aux: should target a loop with nested loop^inside"
      end
    | _ -> begin match Internal.extract_loop body1 with
           | Some (loop2, body2) -> loop2 (trm_seq_nomarks [loop1 body2])
           | None -> fail body1.loc "Loop_core..swap_aux: should target a loop with nested inner loops"
           end
    end
  | None -> fail t.loc "Loop_core.swap_aux: should target a loop"

(* [swap t p] - applies [swap_aux] at trm [t] with path [p] *)
let swap : Transfo.local =
  apply_on_path (swap_aux)

(*  [color_aux nb_colors i_color t]: transform a loop into two nested loops based on the coloring pattern,
      [nb_colors] - a variable used to represent the number of colors,
      [i_color] - a variable representing the index used of the new outer loop,
      [t] - ast of the loop. *)
let color_aux (nb_colors : trm) (i_color : var option) (t : trm) : trm =
  match t.desc with
  | Trm_for (l_range, body) ->
    let (index , start, direction, stop, step, is_parallel) = l_range in
    let i_color = match i_color with
    | Some cl -> cl
    | _ -> "c" ^ index
    in
    let is_step_one =
      begin match step with
      | Post_inc | Pre_inc -> true
      | _ -> false
      end in
   let nb_colors = nb_colors in 
    trm_for (i_color, start, direction, nb_colors, (Post_inc), is_parallel) (
      trm_seq_nomarks [
        trm_for (index, (if is_step_one then trm_var i_color else trm_apps (trm_binop Binop_mul) [trm_var i_color; loop_step_to_trm step]), direction, stop,
          (if is_step_one then Step nb_colors else Step (trm_apps (trm_binop Binop_mul) [nb_colors; loop_step_to_trm step])), is_parallel) body
      ]
    )
  | _ -> fail t.loc "Loop_core.color_aux: only_simple loops are supported"

(* [color nb_colors i_color t p]: applies [color_aux] at trm [t] with path [p] *)
let color (nb_colors : trm) (i_color : var option ) : Transfo.local =
    apply_on_path (color_aux nb_colors  i_color)

(*  [tile_aux divides b tile_index t]: tiles loop [t],
      [tile_index] - string representing the index used for the new outer loop,
      [bound] - a tile_bound type variable representing the type of the bound used in
                 this transformation,
      [t] - ast of targeted loop. *)
let tile_aux (tile_index : var) (bound : tile_bound) (tile_size : trm) (t : trm) : trm =
  match t.desc with
  | Trm_for (l_range, body) ->
    let (index, start, direction, stop, step, is_parallel) = l_range in
    let tile_index = Tools.string_subst "${id}" index tile_index in
    let tile_bound =
     if is_step_one step then trm_add (trm_var tile_index) tile_size else trm_add (trm_var tile_index ) (trm_mul tile_size (loop_step_to_trm step)) in
    let inner_loop =
     begin match bound with
     | TileBoundMin ->
       let tile_bound =
       trm_apps (trm_var "min") [stop; tile_bound] in
       trm_for (index, (trm_var tile_index), direction, (tile_bound), step, is_parallel) body
     | TileBoundDivides ->
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
       let new_body = Internal.change_trm (trm_var index) (trm_var_get index) body in
       trm_for_c init cond step new_body
     end in
     trm_for (tile_index, start, direction, stop, (if is_step_one step then Step tile_size else Step (trm_mul tile_size (loop_step_to_trm step))), is_parallel) (
       trm_seq_nomarks [inner_loop]
     )
  | _ -> fail t.loc "Loop_core.tile_aux: only simple loop are supported "


(* [tile tile_index bound tile_size t p]: applies [tile_aux] at trm [t] with path [p] *)
let tile (tile_index : var) (bound : tile_bound) (tile_size : trm) : Transfo.local =
   apply_on_path (tile_aux tile_index bound tile_size )

(* [hoist_aux name decl_index array_size t]: extracts a variable declared inside a loop as an array of size [loop_bound -1]
    then replace all the occurrences of that variable with an array access at the loop index,
      [name] - pattern of the form "${var}_something" for the name entered by the user, if not
              entered by the user, the dafault pattern ${var}_step is used,
      [t] - ast of the loop. *)
let hoist_aux (name : var) (decl_index : int) (array_size : trm option) (t : trm) : trm =
  match t.desc with
  | Trm_for (l_range, body) ->
    begin match body.desc with
    | Trm_seq tl ->
      let (index, _, _, stop, _, _) = l_range in
      let stop_bd = begin match array_size with | Some arr_sz -> arr_sz | None -> stop end in 
      let lfront, var_decl, lback = Internal.get_trm_and_its_relatives decl_index tl in
      begin match var_decl.desc with
      | Trm_let (vk, (x, tx), _) ->
        let new_name = Tools.string_subst "${var}" x name in
        let new_decl = trm_let_ref (x, (get_inner_ptr_type tx)) (trm_apps (trm_binop Binop_array_access) [trm_var_get new_name; trm_var index] ) in
        let new_tl = Mlist.merge lfront lback in
        let new_body = trm_seq (Mlist.insert_at decl_index new_decl new_tl) in
        let inner_typ = get_inner_ptr_type tx in
        trm_seq_no_brace [
          trm_let_array Var_mutable (new_name, inner_typ) (Trm stop_bd) (trm_uninitialized ());
          trm_for l_range new_body ]
      | _ -> fail var_decl.loc "Loop_core.hoist_aux: expected a variable declaration"
      end
    | _ -> fail t.loc "Loop_core.hoist_aux: body of the loop should be a sequence"
    end
  | _ -> fail t.loc "Loop_core.hoist_aux: only simple loops are supported"


(* [hoist name index array_size t p]: applies [hoist_aux] at trm [t] with path [p]. *)
let hoist (name : var) (index : int) (array_size : trm option): Transfo.local =
   apply_on_path (hoist_aux name index array_size)

(* [fission_aux]: split loop [t] into two loops
    params:
      [index]: index of the splitting point
      [t]: ast of the loop *)
 let fission_aux (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_for (l_range, body) ->
    begin match body.desc with
    | Trm_seq tl ->
      let tl1, tl2 = Mlist.split index tl in
      let b1 = trm_seq tl1 in
      let b2 = trm_seq tl2 in
      trm_seq_no_brace [
        trm_for l_range b1;
        trm_for l_range b2;]
    | _ -> fail t.loc "Loop_core.fission_aux: expected the sequence inside the loop body"
    end
  | _ -> fail t.loc "Loop_core.fission_aux: only simple loops are supported"

(* [fission index t p]: applies [fission_aux] at the trm [t] with path [p]. *)
let fission (index : int) : Transfo.local=
 apply_on_path (fission_aux index)


(* [fusion_on_block_aux t]: merges two or more loops with the same components except the body,
      [t] - ast of the sequence containing the loops. *)
let fusion_on_block_aux (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let n = Mlist.length tl in
    if n < 2 then fail t.loc "fission_aux: there must be >= 2 loops to apply fussion";
    let first_loop = Mlist.nth tl 0 in
     begin match  first_loop.desc with
    | Trm_for (l_range, _) ->
      let fusioned_body = Mlist.fold_lefti (
        fun i acc loop ->
          if not (Internal.is_trm_loop loop) then fail loop.loc (Printf.sprintf "Loop_core.fusion_on_block_aux: cannot
                                                               fuse %d loops as requested only %d where found" n (i+1))
           else
          acc @ (Mlist.to_list (for_loop_body_trms loop))
      ) [] tl in
      trm_for l_range (trm_seq_nomarks fusioned_body)
    | _ -> fail t.loc "Loop_core.fusion_on_block_aux: all loops should be simple loops"
    end
  | _ -> fail t.loc "Loop_core.fission_aux: expected a sequence of for loops"

(* [fusion_on_block keep_label t p]: applies [fusion_on_block_aux t p] at trm [t] with path [p]. *)
let fusion_on_block (keep_label : bool): Transfo.local =
  apply_on_path (Internal.apply_on_path_targeting_a_sequence ~keep_label (fusion_on_block_aux) "fussion")

(* [grid_enumerate_aux indices_and_bounds t]: transforms a loop over a grid into nested loops over 
    each dimension of that grid,
      [indices_and_bounds] - a list of pairs representing the index and the bound for each dimension,
      [t] - ast of the loop. *)
let grid_enumerate_aux (indices_and_bounds : (string * trm) list) (t : trm) : trm =
  match t.desc with
  | Trm_for (l_range, body) ->
    let (index, _, direction, _, _, is_parallel) = l_range in
    let new_body = begin match body.desc with
                   | Trm_seq tl ->
                      let old_loop_index_val = Xlist.fold_lefti (fun i acc (ind, bnd) ->
                        if i = 0 then let acc = trm_var ind in acc
                          else trm_apps (trm_binop Binop_add) [
                            trm_apps (trm_binop Binop_mul) [
                              acc; bnd]
                              ; trm_var ind]
                    )  (trm_unit ()) indices_and_bounds in
                    let old_loop_index_decl = trm_let_immut (index, typ_int ()) old_loop_index_val in
                    let new_tl = Mlist.insert_at 0 old_loop_index_decl tl in
                    trm_seq new_tl
                   | _ -> fail body.loc "Loop_core.grid_enumerate_aux: the body of the loop should be a sequence"
                   end in

    Xlist.fold_lefti (fun i acc (ind, bnd) ->
      if i = 0 then  trm_for (ind, (trm_int 0), direction, bnd, (Post_inc), is_parallel) acc
        else  trm_for (ind, (trm_int 0), DirUp, bnd, Post_inc, false) (trm_seq_nomarks [acc])
    ) new_body (List.rev indices_and_bounds)
  | _ -> fail t.loc "Loop_core.grid_enumerate_aux: expected a simple loop"

(* [grid_enumerate indices_and_bounds t p]: applies [grid_enumerate_aux indices_and_bounds] at trm [t] with path [p]. *)
let grid_enumerate (indices_and_bounds : (string * trm) list) : Transfo.local =
  apply_on_path (grid_enumerate_aux indices_and_bounds)

(* [unroll_aux index t]: unrolls loop [t],
      [braces] - a flag on the visibility of generated sequences,
      [my_mark] - a mark left on the top generated sequence,
      [t] - ast of the loop. *)
let unroll_aux (braces : bool) (my_mark : mark) (t : trm) : trm =
  match t.desc with
  | Trm_for (l_range, body) ->
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
        | _ -> fail t.loc "Loop_core.unroll_aux: the loop which is going to be unrolled shoudl have a bound which is a sum of a variable and a literal"
        end in
      let unrolled_body = List.fold_left ( fun acc i1 ->
        let new_index =
          begin match start.desc with
          | Trm_val (Val_lit (Lit_int n)) -> trm_lit (Lit_int (n + i1))
          | _ -> trm_apps (trm_binop Binop_add) [start; (trm_lit (Lit_int i1))]
          end in
        let body_i = Internal.subst_var index new_index body in
        let body_i = if braces
                      then Internal.remove_nobrace_if_sequence body_i
                      else Internal.set_nobrace_if_sequence body_i in
        body_i :: acc ) [] (List.rev unrolled_loop_range) in
      begin match my_mark with
      | "" -> trm_seq_no_brace unrolled_body
      | _ -> trm_seq_no_brace [trm_add_mark my_mark (trm_seq_no_brace unrolled_body)]
      end
  | _ -> fail t.loc "Loop_core.unroll_aux: only simple loops supported"

(* [unroll braces my_mark t p]: applies [unroll_aux] at trm [t] with path [p]. *)
let unroll (braces : bool)(my_mark : mark) : Transfo.local =
  apply_on_path (unroll_aux braces my_mark)

(* [move_out_aux trm_index t]: moves an invariant instruction just before loop [t]
    params:
      [trm_index]: index of that instruction on its surrouding sequence
      [t]: ast of the for loop *)
let move_out_aux (trm_index : int) (t : trm) : trm =
  match t.desc with
  | Trm_for (l_range, _) ->
    let tl = for_loop_body_trms t in
    let lfront, trm_inv, lback = Internal.get_trm_and_its_relatives trm_index tl in
    trm_seq_no_brace ([trm_inv] @ [
      trm_for l_range (trm_seq (Mlist.merge lfront lback))])
  | Trm_for_c (init, cond, step, _) ->
    let tl = for_loop_body_trms t in
    let lfront, trm_inv, lback = Internal.get_trm_and_its_relatives trm_index tl in
    trm_seq_no_brace  ([trm_inv] @ [
      trm_for_c init cond step (trm_seq (Mlist.merge lfront lback))])
  | _ -> fail t.loc "Loop_core.move_out_aux: expected a loop"

(* [move_out trm_index t p]: applies [move_out_aux] at trm [t] with path [p] *)
let move_out (trm_index : int) : Transfo.local =
  apply_on_path (move_out_aux trm_index)

(* [unswitch_aux trm_index t]: extracts an if statement inside the loop whose condition,
    is not dependent on the index of the loop or any other local variables,
      [trm_index] - index of the if statement inside the body of the loop,
      [t] - ast of the for loop. *)
let unswitch_aux (trm_index : int) (t : trm) : trm =
  let tl = for_loop_body_trms t in
  let if_stmt = Mlist.nth tl trm_index in
  match if_stmt.desc with
  | Trm_if (cond, then_, else_) ->
    let then_ = Internal.set_nobrace_if_sequence then_ in
    let else_ = Internal.set_nobrace_if_sequence else_ in
    let wrap_branch (t1 : trm) : trm  = Internal.change_loop_body t (trm_seq (Mlist.replace_at trm_index t1 tl )) in
    trm_if cond (wrap_branch then_) (wrap_branch else_)
  | _ -> fail if_stmt.loc "Loop_core.unswitch_aux: expected an if statement"

(* [unswitch trm_index t p]: applies [unswitch_aux] at trm [t] with path [p]. *)
let unswitch (trm_index : int) : Transfo.local =
  apply_on_path (unswitch_aux trm_index)
  
(* [to_unit_steps_aux new_index t]: transforms loop [t] into a loop with unit steps,
      [new_index] - a string representing the new index for the transformed loop,
      [t] - ast of the loop to be transformed. *)
let to_unit_steps_aux (new_index : var) (t : trm) : trm =
  match t.desc with
  | Trm_for (l_range, _) ->
    let (index, start, direction, stop, step, is_parallel) = l_range in
    let new_index = match new_index with
    | "" -> index ^ "_step"
    | _ -> new_index in

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
  | _ -> fail t.loc "Loop_core.to_unit_steps: only simple loops are supported "

(* [loop_to_unit_steps new_index t p]: applies [to_unit_steps_aux] to the trm [t] with path [p]. *)
let to_unit_steps (new_index : var) : Transfo.local =
  apply_on_path (to_unit_steps_aux new_index)


(* [fold_aux index start step t]: transforms a sequence of instructions into a for loop,
      [index] - index of the generated for loop,
      [start] - starting value for the index of the generated for loop,
      [step] - step of the generated for loop,
      [t] - ast of the sequence.
      
    NOTE: we trust the user that "stop" corresponds to the number of iterations
    LATER: use  sExpr  to mark the subexpression that correspnod to the string "start";
    then you can Generic.replace at these marks.*)
let fold_aux (index : var) (start : int) (step : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
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
    trm_for (index, (trm_int start), DirUp, (trm_int nb), (if step = 1 then Post_inc else Step (trm_int step)), false) (trm_seq_nomarks [loop_body])
  | _ -> fail t.loc "Loop_core.fold_aux: expected a sequence of instructions"

(* [fold index start step t p]: applies [fold_aux] at trm [t] with path [p]. *)
let fold (index : var) (start : int) (step : int) : Transfo.local =
  apply_on_path (fold_aux index start step)

(* [split_range_aux nb cut]: splits a loop into two loops based on the range,
      [nb] - by default this argument has value 0, if provided it means that it will split the loop at start + nb iteration,
      [cut] - by default this argument has value tmr_unit(), if provided then the loop will be splited at that iteration,
      [t] - ast of the for loop. *)
let split_range_aux (nb : int)(cut : trm)(t : trm) : trm = 
  match t.desc with 
  | Trm_for (l_range, body) -> 
     let (index, start, direction, stop, step, is_parallel) = l_range in
     let split_index = 
     begin match nb, cut with 
     | 0, {desc = Trm_val (Val_lit (Lit_unit )); _} -> fail t.loc "Loop_core.split_range_aux: one of the args nb or cut should be set "
     | 0, _ -> cut
     | _, {desc = Trm_val (Val_lit (Lit_unit ));_} -> trm_add (trm_var index) (trm_lit (Lit_int nb))
     | n, c -> fail t.loc "Loop_core.split_range_aux: can't provide both the nb and cut args"
     end in 
     trm_seq_no_brace [
       trm_for (index, start, direction, split_index, step, is_parallel) body;
       trm_for (index, split_index, direction, stop, step, is_parallel) body]
     
  | _ -> fail t.loc "split_range_aux: expected a target to a simple for loop"


(* [split_range nb cut t p]: applies [split_range_aux] at the trm [t] with path [p]. *)
let split_range (nb : int) (cut : trm) : Transfo.local =
  apply_on_path (split_range_aux nb cut)
