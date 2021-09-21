open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [interchange_aux t]: swap the order of two nested loops, the targeted loop
      the immediate inner loop
    params:
      t: ast of the targeted loop
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
      | None -> fail body1.loc "internchange_aux: should target a loop with nested loop^inside"
      end
    | _ -> fail body1.loc "interchange_aux: body of the loop should be a sequence"
    end
  | None -> fail t.loc "interchange_aux: should target a loop"

(* let interchange_aux (t : trm) : trm =
  match Internal.extract_loop t with
  | Some (loop1, body1) ->
    begin match body1.desc with
    | Trm_seq [loop2] ->
       begin match Internal.extract_loop loop2 with
      | Some (loop2, body2) -> loop2 (trm_seq [(loop1 body2)])
      | None -> fail body1.loc "interchange_aux: should target a loop with nested loop inside"
      end
    | _ -> fail body1.loc "interchange_aux: body of the loop should be a sequence"
    end
  | None -> fail t.loc "interchange_aux: should target a loop" *)


let interchange : Target.Transfo.local =
  Target.apply_on_path (interchange_aux)


(*  [color_aux nb_colors i_color t]: transform a loop into two nested loops respecting based
        on coloring pattern
      params:
        nb_colors: a variable used to represent the number of colors
        i_color: string used to represent the index used of the new outer loop
        t: ast of the loop
      return:
        updated ast with the transformed loop
*)
let color_aux (nb_colors : var) (i_color : var) (t : trm) : trm =
  (* Ast_to_text.print_ast ~only_desc:true stdout t; *)
  match t.desc with
  | Trm_for (index, direction, start, stop, step, body) ->
    let i_color = match i_color with
      | "" -> "c" ^ index
      | _ -> i_color in
    let full_ast = Trace.get_ast () in
    let i_color = begin match Internal.toplevel_decl i_color full_ast with
    | Some _ -> let rnd_nb = Random.int 100 in i_color ^ (string_of_int rnd_nb)
    | None -> i_color
    end in
    let is_step_equal_one = begin match step.desc with
                            | Trm_val (Val_lit (Lit_int 1)) -> true
                            | _ -> false
                            end in
    trm_for (i_color) direction start (trm_var nb_colors) (trm_lit (Lit_int 1)) (
      trm_seq_nomarks [
        trm_for index direction (if is_step_equal_one then trm_var i_color else trm_apps (trm_binop Binop_mul) [trm_var i_color; step]) stop
         (if is_step_equal_one then  trm_var nb_colors else trm_apps (trm_binop Binop_mul) [trm_var nb_colors; step]) body
      ]
    )
  | _ -> fail t.loc "color_aux: only simple loops are supported"


let color (c : var) (i_color : var) : Target.Transfo.local =
    Target.apply_on_path (color_aux c i_color)

(*  [tile_aux divides b tile_index t]: tile loop t
      params:
        tile_size: a variable used to represent the block size
        tile_index: string used to represent the index used for the new outer loop
        t: ast of the loop going to be tiled
      return:
        updated ast with the tiled loop
*)
let tile_aux (tile_index : var) (bound : tile_bound) (tile_size : var) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, direction, start, stop, step, body) ->
     let tile_index = match tile_index with
      | "" -> "b" ^ index
      | _ -> tile_index in
    (* Hack for elminating the appearance of 1 in the case when step is equal to one *)
    let trm_tile_size =  match step.desc with
    | Trm_val (Val_lit (Lit_int 1)) ->
      trm_var (tile_size)
    | _ -> trm_apps (trm_binop Binop_mul) [trm_var tile_size; step] in
     begin match bound with
      | TileBoundMin ->
        let tile_stop = trm_apps (trm_var "min")[ stop; trm_apps (trm_binop Binop_add)[
          trm_var tile_index;
          trm_apps ~annot:[Mutable_var_get](trm_unop Unop_get) [trm_tile_size]]] in
          trm_for tile_index direction start stop (trm_tile_size) (
              trm_seq_nomarks [
                trm_for index direction (trm_var tile_index) tile_stop step body])
      | TileBoundAnd ->
          let init = trm_let Var_mutable (index, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_int ())) (trm_apps (trm_prim ~loc:start.loc (Prim_new (typ_int ()))) [trm_var tile_index]) in
            let cond = begin match direction with
              | DirUp ->
                trm_apps (trm_binop Binop_and)[
                  trm_apps (trm_binop (Binop_lt)) [trm_var index;
                    trm_apps (trm_binop Binop_add) [
                      trm_var tile_index; trm_tile_size]];
                  trm_apps (trm_binop Binop_lt)  [trm_var index; stop]
                ]
              | DirDown ->
                trm_apps (trm_binop Binop_and)[
                  trm_apps (trm_binop (Binop_gt)) [trm_var index;
                    trm_apps (trm_binop Binop_add) [
                      trm_var tile_index; trm_tile_size]];
                  trm_apps (trm_binop Binop_gt)  [trm_var index; stop]
                ]
              end in
          let step =
            begin match direction with
            | DirUp ->
                begin match step.desc with
                | Trm_val (Val_lit (Lit_int 1)) -> trm_apps (trm_unop Unop_post_inc) [trm_var index]
                | _ ->
                  trm_set (trm_var index ) ~annot:[App_and_set](trm_apps (trm_binop Binop_add)[
                    trm_var index;
                    trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [step]])
                end
            | DirDown ->
                begin match step.desc with
                | Trm_val (Val_lit (Lit_int 1)) -> trm_apps (trm_unop Unop_post_dec) [trm_var index]
                | _ ->
                  trm_set (trm_var index ) ~annot:[App_and_set](trm_apps (trm_binop Binop_sub)[
                    trm_var index;
                    trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [step]])
                end
            end in
            trm_for tile_index direction start stop (trm_tile_size)
              (trm_seq_nomarks [trm_for_c init cond step body])
      | TileBoundDivides ->
        let tile_stop = trm_apps (trm_binop Binop_add)[
                          trm_var tile_index;
                          trm_apps ~annot:[Mutable_var_get](trm_unop Unop_get) [trm_tile_size]] in
        trm_for tile_index direction start stop (trm_tile_size) (
              trm_seq_nomarks [
                trm_for index direction (trm_var tile_index) tile_stop step body])
     end

  | _ -> fail t.loc "tile_aux: only simple loops are supported"

let tile (tile_index : var) (bound : tile_bound) (tile_size : var) : Target.Transfo.local =
   Target.apply_on_path (tile_aux tile_index bound tile_size )



(* [hoist_aux x_step t]: extract a loop variable inside the loop as an array with size equal
      to (loop_bound - 1), the change all the occurrences of the variable with an array access
      with index same as the index of the loop
    params:
      x_step: a new_variable name going to appear as the extract variable
      t: ast of the loop
    return:
      updated ast with the hoisted loop
*)
let hoist_aux (x_step : var) (decl_index : int) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, direction, start, stop, step, body) ->
    begin match body.desc with
    | Trm_seq tl ->
      (* We assume that the first elment in the body is a variable declaration *)
      let var_decl = Mlist.nth tl decl_index in
      begin match var_decl.desc with
      | Trm_let (vk, (x, tx), _) ->
        let new_decl = trm_let vk (x, typ_ptr Ptr_kind_ref (get_inner_ptr_type tx)) (trm_apps (trm_binop Binop_array_cell_addr) [trm_var x_step; trm_var index] ) in
        let new_body = trm_seq (Mlist.insert_at decl_index new_decl tl) in
        let inner_typ = get_inner_ptr_type tx in
        trm_seq_no_brace [
          trm_let Var_mutable (x_step, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_array inner_typ (Trm stop))) (trm_prim (Prim_new inner_typ));
          trm_for index direction start stop step new_body ]
      | _ -> fail var_decl.loc "hoist_aux: expected a variable declaration"
      end
    | _ -> fail t.loc "hoist_aux: body of the loop should be a sequence"
    end


  | _ -> fail t.loc "hoist_aux: only simple loops are supported"


let hoist (x_step : var) (index : int): Target.Transfo.local =
   Target.apply_on_path (hoist_aux x_step index)

(* [extract_variable_aux decl_index t] similar to loop hoist *)
let extract_variable_aux (decl_index : int) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, direction, start, stop, step, body) ->
    begin match body.desc with
    | Trm_seq tl ->
      let lfront, var_decl, lback = Internal.get_trm_and_its_relatives decl_index tl in
      begin match var_decl.desc with
      | Trm_let (_, (x, tx), _) ->
        let lback = Mlist.map (
          Internal.change_trm (trm_var x)
          (trm_apps (trm_binop Binop_array_cell_addr) [trm_var x; trm_var index] )
        ) lback in
        trm_seq_no_brace [
          trm_let Var_mutable (x, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_array (get_inner_ptr_type tx) (Trm stop))) (trm_prim (Prim_new (typ_array (get_inner_ptr_type tx) (Trm stop))));
          trm_for index direction start stop step (trm_seq ~annot:body.annot (Mlist.merge lfront lback))
        ]
      | _ -> fail var_decl.loc "extract_variable_aux: expected the declaration of the variable to be extracted"
      end
    | _ -> fail body.loc "exptract_variable_aux: body of the loop should be a sequence"
    end
  | _ -> fail t.loc "extract_variable_aux: expected a for loop"


let extract_variable (index : int) : Target.Transfo.local =
  Target.apply_on_path(extract_variable_aux index)

(* [fission_aux]: split a loop into two loops
    params:
      index: index of the splitting point inside the body of the loop
      t: ast of the loop
    return
      updated ast with the splitted loop
 *)
 let fission_aux (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_for (loop_index, direction, start, stop, step, body) ->
    begin match body.desc with
    | Trm_seq tl ->

      let first_part, last_part = Tools.split_list_at index (Mlist.to_list tl) in
      let first_body = trm_seq_nomarks first_part in
      let second_body = trm_seq_nomarks last_part in
      trm_seq_no_brace [
        trm_for loop_index direction start stop step first_body;
        trm_for loop_index direction start stop step second_body;]
    | _ -> fail t.loc "fission_aux: expected the sequence inside the loop body"
    end
  | _ -> fail t.loc "fission_aux: onl simple loops are supported"

 let fission (index : int) : Target.Transfo.local=
  Target.apply_on_path (fission_aux index)


(* [fusion_on_block_aux t]: merge two loops with the same components except the body
    params:
      t: ast of the sequence containing the loops
    return
      update ast with the merged loops
 *)
let fusion_on_block_aux (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let n = Mlist.length tl in
    if n < 2 then fail t.loc "fission_aux: there must be >= 2 loops to apply fussion";
    let first_loop = Mlist.nth tl 0 in
     begin match  first_loop.desc with
    | Trm_for (index, direction, start, stop, step, _) ->
      let fusioned_body = Mlist.foldi (
        fun i acc loop -> 
          if not (Internal.is_trm_loop loop) then fail loop.loc (Tools.sprintf "fusion_on_block_aux: cannot fuse %d loops as requested only %d where found" n (i+1)) 
           else
          acc @ (Mlist.to_list (for_loop_body_trms loop))
      ) [] tl in
      trm_for index direction start stop step (trm_seq_nomarks fusioned_body)
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
      updated ast with the transformed loop
*)
let grid_enumerate_aux (index_and_bounds : (string * string) list) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, direction, _start, _stop, _step, body) ->
    let new_body = begin match body.desc with
                   | Trm_seq tl ->
                      let old_loop_index_val = Tools.foldi (fun i acc (ind, bnd) ->
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
    Tools.foldi (fun i acc (ind, bnd) ->
      if i = 0 then  trm_for ind direction (trm_lit (Lit_int 0)) (trm_var bnd) (trm_lit (Lit_int 1)) acc
        else  trm_for ind direction (trm_lit (Lit_int 0)) (trm_var bnd) (trm_lit (Lit_int 1)) (trm_seq_nomarks [acc])
    ) new_body (List.rev index_and_bounds)
  | _ -> fail t.loc "grid_enumerate_aux: expected a simple loop"


let grid_enumerate (index_and_bounds : (string * string) list) : Target.Transfo.local =
  Target.apply_on_path (grid_enumerate_aux index_and_bounds)




(* [unroll_aux index t]: extract the body of the loop as a list of list of instructions
    params:
      [index]: index of the loop inside the sequence containing the loop
      [t]: ast of the loop
    return:
      updated ast with the unrolled loop
*)
let unroll_aux (label : var) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, _direction, _start, stop, _step, body) ->
      let unroll_bound = begin match stop.desc with
                         | Trm_apps(_,[_; bnd]) ->
                            begin match bnd.desc with
                            | Trm_val (Val_lit (Lit_int bnd)) -> bnd
                            | _ -> fail bnd.loc "unroll_aux: expected a literal trm"
                            end
                         | _ -> fail t.loc "unroll_aux: the loop which is going to be unrolled shoudl have a bound which is a sum of a variable and a literal"
                         end in
      let unrolled_loop_range = Tools.range 0 (unroll_bound - 1) in
      let unrolled_body = List.fold_left ( fun acc i1 ->
        let new_index = Internal.change_trm (trm_lit (Lit_int unroll_bound)) (trm_lit (Lit_int i1)) stop in
        Internal.change_trm (trm_var index) new_index body :: acc
         ) [] (List.rev unrolled_loop_range) in
      begin match label with
      | "" -> trm_seq_no_brace unrolled_body 
      | _ -> trm_seq_no_brace [trm_labelled label (trm_seq_no_brace unrolled_body)] 
      end
  | _ -> fail t.loc "unroll_aux: only simple loops supported"


let unroll (label : var) : Target.Transfo.local =
  Target.apply_on_path (unroll_aux label)
(* [invariant_aux trm_index t]: take a constant term inside the body of the loop
      in outside the loop.
    params:
      [trm_index]: index of the constant trm inside the body of the loop
      [t]: ast of the loop
    return:
      updated ast with the extracted constant trm outside the loop
*)
let invariant_aux (trm_index : int) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, direction, start, stop, step, _) ->
    let tl = for_loop_body_trms t in
    let lfront, trm_inv, lback = Internal.get_trm_and_its_relatives trm_index tl in
    trm_seq_nomarks ~annot: [No_braces (Nobrace.current())] ([trm_inv] @ [
      trm_for index direction start stop step (trm_seq (Mlist.merge lfront lback))])
  | Trm_for_c (init, cond, step, _) ->
    let tl = for_loop_body_trms t in
    let lfront, trm_inv, lback = Internal.get_trm_and_its_relatives trm_index tl in
    trm_seq_nomarks ~annot: [No_braces (Nobrace.current())]  ([trm_inv] @ [
      trm_for_c init cond step (trm_seq (Mlist.merge lfront lback))])
  | _ -> fail t.loc "invariant_aux: expected a loop"

let invariant (trm_index : int) : Target.Transfo.local =
  Target.apply_on_path (invariant_aux trm_index)

(* [unswitch_aux trm_index t]: extract and if statement inside the loop which is not
        dependent on the index of the loop ofr any local variables outside the loop.
      params:
        trm_index: index of the if statement inside the body of the loop
        t: ast of the for loop to be transformed
      return:
        updated ast with the extracted if statement
*)
let unswitch_aux (trm_index : int) (t : trm) : trm =
  let tl = for_loop_body_trms t in
  let if_stmt = Mlist.nth tl trm_index in
  match if_stmt.desc with
  | Trm_if (cond, then_, else_) ->
    let then_ = Internal.set_no_brace_if_sequence then_ in
    let else_ = Internal.set_no_brace_if_sequence else_ in
    let wrap_branch (t1 : trm) : trm  = Internal.change_loop_body t (trm_seq (Mlist.replace_at trm_index t1 tl )) in
    trm_if cond (wrap_branch then_) (wrap_branch else_)
  | _ -> fail if_stmt.loc "unswitch_aux: expected an if statement"

let unswitch (trm_index : int) : Target.Transfo.local =
  Target.apply_on_path (unswitch_aux trm_index)


(* [to_unit_steps_aux new_index t]: transform a loop into a loop with unit steps
     params:
      new_index: a string representing the new index for the transformed loop
      t: ast of the loop to be transformed
     return:
      updated ast with the transformed loop
*)
let to_unit_steps_aux (new_index : var) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, direction, start, stop, step, _) ->
    let new_index = match new_index with
    | "" -> index ^ "_step"
    | _ -> new_index in
    let body_trms = for_loop_body_trms t in
    let new_decl = trm_let Var_mutable (index, (typ_ptr Ptr_kind_mut (typ_int()) ~typ_attributes:[GeneratedStar]))
        (trm_apps (trm_prim (Prim_new (typ_int())))
          [trm_apps (trm_binop Binop_add)[
            start;
            trm_apps (trm_binop Binop_mul) [trm_var new_index; step]
          ]]) in
    trm_for new_index direction (trm_lit (Lit_int 0))
      (trm_apps (trm_binop Binop_div) [trm_apps (trm_binop Binop_sub) [stop; start]; step])
      (trm_lit (Lit_int 1))
      (trm_seq (Mlist.insert_at 0 new_decl body_trms ))

  | _ -> fail t.loc "to_unit_steps_aux: expected a simple for loop"

let to_unit_steps (new_index : var) : Target.Transfo.local =
  Target.apply_on_path (to_unit_steps_aux new_index)

