open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [interchange_aux t]: swap the order of two nested loops, the targeted loop
      the immediate inner loop
    params:
      [t]: ast of the targeted loop
    return:
      updated ast with swapped loops
 *)
let interchange_aux (t : trm) : trm =
  match Internal.extract_loop t with
  | Some (loop1, body1) ->
    begin match body1.desc with
    | Trm_seq tl when Mlist.length tl = 1 ->
      let loop2 = Mlist.nth tl 0 in
      begin match Internal.extract_loop loop2 with
      | Some (loop2, body2) -> loop2 (trm_seq_nomarks [loop1 body2])
      | None -> fail body1.loc "interchange_aux: should target a loop with nested loop^inside"
      end
    | _ -> begin match Internal.extract_loop body1 with
           | Some (loop2, body2) -> loop2 (trm_seq_nomarks [loop1 body2])
           | None -> fail body1.loc "interchange_aux: should target a loop with nested inner loops"
           end
    end
  | None -> fail t.loc "interchange_aux: should target a loop"

let interchange : Target.Transfo.local =
  Target.apply_on_path (interchange_aux)


(*  [color_aux nb_colors i_color t]: transform a loop into two nested loops based
        on the coloring pattern
      params:
        [nb_colors]: a variable used to represent the number of colors
        [i_color]: a variable representing the index used of the new outer loop
        [t]: ast of the loop
      return:
        the transformed loop
*)


let color_aux (nb_colors : var) (i_color : var option) (t : trm) : trm = 
  match t.desc with 
  | Trm_for (index , start, stop, step, body) ->
  
    let i_color = match i_color with 
    | Some cl -> cl 
    | _ -> "c" ^ index
    in
    let is_step_one = 
      begin match step with 
      | Post_inc | Pre_inc -> true 
      | _ -> false 
      end in
  
    trm_for i_color start (DirUp (trm_var nb_colors)) (Post_inc) (
      trm_seq_nomarks [
        trm_for index (if is_step_one then trm_var i_color else trm_apps (trm_binop Binop_mul) [trm_var i_color; loop_step_to_trm step]) stop 
          (if is_step_one then Step (trm_var nb_colors) else Step (trm_apps (trm_binop Binop_mul) [trm_var nb_colors; loop_step_to_trm step])) body
      ]
    )

  | _ -> fail t.loc "color_aux: only_simple loops are supported"

let color (c : var) (i_color : var option ) : Target.Transfo.local =
    Target.apply_on_path (color_aux c  i_color)

(*  [tile_aux divides b tile_index t]: tile loop t
      params:
        [bound]: a tile_bound type variable representing the type of the bound used in
          this transformation
        [tile_index]: string used to represent the index used for the new outer loop
        [t]: ast of the loop going to be tiled
      return:
        the tiled loop
*)

let tile_aux (tile_index : var) (bound : tile_bound) (tile_size : var) (t : trm) : trm =
  match t.desc with 
  | Trm_for (index, start, stop, step, body) -> 
   let tile_index = Str.global_replace (Str.regexp_string "${id}") index tile_index in
   let loop_bound, loop_cond  = begin match stop with 
   | DirUp bnd -> bnd, trm_apps (trm_binop Binop_lt ) [bnd]
   | DirUpEq bnd -> bnd, trm_apps (trm_binop Binop_le ) [bnd]
   | DirDown bnd -> bnd, trm_apps (trm_binop Binop_gt ) [bnd]
   | DirDownEq bnd -> bnd, trm_apps (trm_binop Binop_ge ) [bnd] end in
    let tile_bound = begin match bound with 
      | TileBoundMin -> 
         trm_apps (trm_var "min") [loop_bound; trm_apps (trm_binop Binop_add) [
           trm_var tile_index; trm_var tile_size]] 
      | TileBoundAnd -> 
        
        
        trm_apps (trm_binop Binop_and ) [
            trm_apps (trm_binop Binop_add) [trm_var tile_index; trm_var tile_size];
            loop_cond
            ]
      | TileBoundDivides -> 
        trm_apps (trm_binop Binop_add) [trm_var tile_index; trm_var tile_size]
    end in
    trm_for tile_index start (DirUp loop_bound) (Step (trm_var tile_size)) (trm_seq_nomarks [
      trm_for index (trm_var tile_index) (DirUp tile_bound) step body
    ])
  | _ -> fail t.loc "tile_aux: only simple loops are supported "



let tile (tile_index : var) (bound : tile_bound) (tile_size : var) : Target.Transfo.local =
   Target.apply_on_path (tile_aux tile_index bound tile_size )



(* [hoist_aux name t]: extract a loop variable inside the loop as an array with size equal
      to (loop_bound - 1), the change all the occurrences of the variable with an array access
      with index same as the index of the loop
    params:
      [name]: a pattern of the form ${var}_something for the name entered by the user otherwise used the dafault pattern
        ${var}_step
      [t]: ast of the loop
    return:
      a nobrace sequence which contains the hoisted variable and the for loop
*)
let hoist_aux (name : var) (decl_index : int) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, start, stop, step, body) ->
    
    begin match body.desc with
    | Trm_seq tl ->
      let lfront, var_decl, lback = Internal.get_trm_and_its_relatives decl_index tl in
      begin match var_decl.desc with
      | Trm_let (vk, (x, tx), _) ->
        let new_name = Str.global_replace (Str.regexp_string "${var}") x name in
        let new_decl = trm_let vk (x, typ_ptr Ptr_kind_ref (get_inner_ptr_type tx)) (trm_apps (trm_binop Binop_array_cell_addr) [trm_var new_name; trm_var index] ) in
        let new_tl = Mlist.merge lfront lback in
        let new_body = trm_seq (Mlist.insert_at decl_index new_decl new_tl) in
        let inner_typ = get_inner_ptr_type tx in
        let trm_stop = begin match stop with | DirUp bnd | DirUpEq bnd | DirDown bnd | DirDownEq bnd -> bnd end in
        trm_seq_no_brace [
          trm_let Var_mutable (new_name, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_array inner_typ (Trm trm_stop))) (trm_prim (Prim_new inner_typ));
          trm_for index start stop step new_body ]
      | _ -> fail var_decl.loc "hoist_aux: expected a variable declaration"
      end
    | _ -> fail t.loc "hoist_aux: body of the loop should be a sequence"
    end


  | _ -> fail t.loc "hoist_aux: only simple loops are supported"


let hoist (name : var) (index : int): Target.Transfo.local =
   Target.apply_on_path (hoist_aux name index)


(* [fission_aux]: split a loop into two loops
    params:
      [index]: index of the splitting point inside the body of the loop
      [t]: ast of the loop
    return
      the splitted loop
 *)
 let fission_aux (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_for (loop_index, start, stop, step, body) ->
    begin match body.desc with
    | Trm_seq tl ->
      let tl1, tl2 = Mlist.split index tl in
      let b1 = trm_seq tl1 in
      let b2 = trm_seq tl2 in
      trm_seq_no_brace [
        trm_for loop_index start stop step b1;
        trm_for loop_index start stop step b2;]
    | _ -> fail t.loc "fission_aux: expected the sequence inside the loop body"
    end
  | _ -> fail t.loc "fission_aux: onl simple loops are supported"

 let fission (index : int) : Target.Transfo.local=
  Target.apply_on_path (fission_aux index)


(* [fusion_on_block_aux t]: merge two loops with the same components except the body
    params:
      [t]: ast of the sequence containing the loops
    return
      merged loops
 *)

let fusion_on_block_aux (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let n = Mlist.length tl in
    if n < 2 then fail t.loc "fission_aux: there must be >= 2 loops to apply fussion";
    let first_loop = Mlist.nth tl 0 in
     begin match  first_loop.desc with
    | Trm_for (index, start, stop, step, _) ->
      let fusioned_body = Mlist.fold_lefti (
        fun i acc loop ->
          if not (Internal.is_trm_loop loop) then fail loop.loc (Tools.sprintf "fusion_on_block_aux: cannot fuse %d loops as requested only %d where found" n (i+1))
           else
          acc @ (Mlist.to_list (for_loop_body_trms loop))
      ) [] tl in
      trm_for index start stop step (trm_seq_nomarks fusioned_body)
    | _ -> fail t.loc "fusion_on_block_aux: all loops should be simple loops"
    end
  | _ -> fail t.loc "fission_aux: expected a sequence of for loops"


let fusion_on_block (keep_label : bool): Target.Transfo.local =
  Target.apply_on_path (Internal.apply_on_path_targeting_a_sequence ~keep_label (fusion_on_block_aux) "fussion")

(* [grid_enumerate_aux index_and_bounds t]: transform a loop over a grid into ndested loops over each dimension
      of the grid
    params:
      [index_and_bounds]: a list of pairs representing the index and the bound of the loop over each dimesnion
      [t]: ast of the loop
    return:
      the transformed loop
*)
let grid_enumerate_aux (index_and_bounds : (string * string) list) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, _start, _stop, _step, body) ->
    let new_body = begin match body.desc with
                   | Trm_seq tl ->
                      let old_loop_index_val = Tools.fold_lefti (fun i acc (ind, bnd) ->
                        if i = 0 then let acc = trm_var ind in acc
                          else trm_apps (trm_binop Binop_add) [
                            trm_apps (trm_binop Binop_mul) [
                              acc; trm_var bnd]
                              ; trm_var ind]
                    )  (trm_var "") index_and_bounds in
                    let old_loop_index_decl = trm_let Var_mutable (index, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_int ())) old_loop_index_val in
                    let new_tl = Mlist.insert_at 0 old_loop_index_decl tl in
                    trm_seq new_tl
                   | _ -> fail body.loc "grid_enumerate_aux: the body of the loop should be a sequence"
                   end in
    
    Tools.fold_lefti (fun i acc (ind, bnd) ->
      if i = 0 then  trm_for ind (trm_int 0) (DirUp (trm_var bnd)) (Post_inc) acc
        else  trm_for ind (trm_int 0) (DirUp (trm_var bnd)) Post_inc (trm_seq_nomarks [acc])
    ) new_body (List.rev index_and_bounds)
  | _ -> fail t.loc "grid_enumerate_aux: expected a simple loop"


let grid_enumerate (index_and_bounds : (string * string) list) : Target.Transfo.local =
  Target.apply_on_path (grid_enumerate_aux index_and_bounds)




(* [unroll_aux index t]: extract the body of the loop as a list of list of instructions
    params:

      [braces]: a flag on the visibility of the sequences generated
      [my_mark]: a mark left on the top generated sequence
      [t]: ast of the loop
    return:
      a sequence of sequences
*)
let unroll_aux (braces : bool) (my_mark : mark) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, start, stop, _step, body) ->
      
      let unrolled_loop_range = begin match (loop_stop_to_trm stop).desc with
                         | Trm_apps(_,[_; bnd]) ->
                            begin match bnd.desc with
                            | Trm_val (Val_lit (Lit_int bnd)) ->
                              Tools.range 0 (bnd - 1)
                            | _ -> fail bnd.loc "unroll_aux: expected a literal trm"
                            end
                          | Trm_val (Val_lit (Lit_int bnd)) ->
                              begin match start.desc with
                              | Trm_val (Val_lit (Lit_int strt)) ->
                                Tools.range 0 (bnd - 1 - strt)
                              | _ -> fail start.loc "unroll_aux: expected a "
                              end
                         | _ -> fail t.loc "unroll_aux: the loop which is going to be unrolled shoudl have a bound which is a sum of a variable and a literal"
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
  | _ -> fail t.loc "unroll_aux: only simple loops supported"


let unroll (braces : bool)(my_mark : mark) : Target.Transfo.local =
  Target.apply_on_path (unroll_aux braces my_mark)


(* [invariant_aux trm_index t]: take a constant term inside the body of the loop
      in outside the loop.
    params:
      [trm_index]: index of the constant trm inside the body of the loop
      [t]: ast of the loop
    return:

*)
let invariant_aux (trm_index : int) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, start, stop, step, _) ->
    let tl = for_loop_body_trms t in
    let lfront, trm_inv, lback = Internal.get_trm_and_its_relatives trm_index tl in
    trm_seq_no_brace ([trm_inv] @ [
      trm_for index start stop step (trm_seq (Mlist.merge lfront lback))])
  | Trm_for_c (init, cond, step, _) ->
    let tl = for_loop_body_trms t in
    let lfront, trm_inv, lback = Internal.get_trm_and_its_relatives trm_index tl in
    trm_seq_no_brace  ([trm_inv] @ [
      trm_for_c init cond step (trm_seq (Mlist.merge lfront lback))])
  | _ -> fail t.loc "invariant_aux: expected a loop"

let invariant (trm_index : int) : Target.Transfo.local =
  Target.apply_on_path (invariant_aux trm_index)

(* [unswitch_aux trm_index t]: extract and if statement inside the loop which is not
        dependent on the index of the loop ofr any local variables outside the loop.
      params:
        [trm_index]: index of the if statement inside the body of the loop
         [t]: ast of the for loop to be transformed
      return:
        updated ast with the extracted if statement
*)
let unswitch_aux (trm_index : int) (t : trm) : trm =
  let tl = for_loop_body_trms t in
  let if_stmt = Mlist.nth tl trm_index in
  match if_stmt.desc with
  | Trm_if (cond, then_, else_) ->
    let then_ = Internal.set_nobrace_if_sequence then_ in
    let else_ = Internal.set_nobrace_if_sequence else_ in
    let wrap_branch (t1 : trm) : trm  = Internal.change_loop_body t (trm_seq (Mlist.replace_at trm_index t1 tl )) in
    trm_if cond (wrap_branch then_) (wrap_branch else_)
  | _ -> fail if_stmt.loc "unswitch_aux: expected an if statement"

let unswitch (trm_index : int) : Target.Transfo.local =
  Target.apply_on_path (unswitch_aux trm_index)


(* [to_unit_steps_aux new_index t]: transform a loop into a loop with unit steps
     params:
      [new_index]: a string representing the new index for the transformed loop
      [t]: ast of the loop to be transformed
     return:
      updated ast with the transformed loop
*)
let to_unit_steps_aux (new_index : var) (t : trm) : trm = 
  match t.desc with 
  | Trm_for (index, start, stop, step, _) -> 
    let new_index = match new_index with
    | "" -> index ^ "_step"
    | _ -> new_index in


   let body_trms = for_loop_body_trms t in 
   let loop_step = match step with 
   | Step l_step -> l_step 
   | _ -> trm_int 1 in

    let new_stop  = 
    begin match stop with
    | DirUp bnd ->  DirUp (trm_apps (trm_binop Binop_div) [trm_apps (trm_binop Binop_sub) [bnd; start]; loop_step])
    | DirUpEq bnd -> DirUpEq (trm_apps (trm_binop Binop_div) [trm_apps (trm_binop Binop_sub) [bnd; start]; loop_step])
    | DirDown bnd -> DirDown (trm_apps (trm_binop Binop_div) [trm_apps (trm_binop Binop_sub) [bnd; start]; loop_step])
    | DirDownEq bnd -> DirDownEq (trm_apps (trm_binop Binop_div) [trm_apps (trm_binop Binop_sub) [bnd; start]; loop_step])
    end in
    let new_decl = trm_let Var_mutable (index, (typ_ptr Ptr_kind_mut (typ_int()) ~typ_attributes:[GeneratedStar]))
        (trm_apps (trm_prim (Prim_new (typ_int())))
          [trm_apps (trm_binop Binop_add)[
            start;
            trm_apps (trm_binop Binop_mul) [trm_var new_index; loop_step]
          ]]) in
    trm_for new_index (trm_int 0) new_stop Post_inc 
      (trm_seq (Mlist.insert_at 0 new_decl body_trms ))
  | _ -> fail t.loc "to_unit_steps: only simple loops are supported "

let to_unit_steps (new_index : var) : Target.Transfo.local =
  Target.apply_on_path (to_unit_steps_aux new_index)

(* NOTE: we trust the user that "stop" corresponds to the number of iterations *)
(* NOTE: currently the function only works for "start = 0"
   LATER: use  sExpr  to mark the subexpression that correspnod to the string "start";
    then you can Generic.replace at these marks *)


(* [loop_fold_aux index start step t]: transform a sequence of instrucitons into a
    single loop with components [index], [start], [nb_instr], [step] and [t].
    params:
      [index]: index of the for generated for loop
      [start]: starting value for the index of the generated for loop
      [step]: step of the generated for loop
      [t]: the ast of the sequence with a list of instructions
    return:
      the ast of the for loop

*)
let fold_aux (index : var) (start : int) (step : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let nb = Mlist.length tl in
    if nb = 0
      then fail t.loc "fold_aux: expected a non-empty list of instructions";
    let first_instr, other_instr  = Tools.uncons (Mlist.to_list tl) in
    let loop_body = Internal.change_trm (trm_int start) (trm_var index) first_instr in
    List.iteri( fun i t1 ->
      let local_body = Internal.change_trm (trm_int (i+1)) (trm_var index) t1 in 
      if not (Internal.same_trm loop_body local_body)
        then fail t1.loc "fold_aux: all the instructions should have the same shape but differ by the index";
    ) other_instr;
    trm_for index (trm_int start) (DirUp (trm_int nb)) (if step = 1 then Post_inc else Step (trm_int step)) (trm_seq_nomarks [loop_body])
  | _ -> fail t.loc "fold_aux: expected a sequence of instructions"
let fold (index : var) (start : int) (step : int) : Target.Transfo.local =
  Target.apply_on_path (fold_aux index start step)
