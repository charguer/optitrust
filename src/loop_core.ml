open Ast


(* [swap_aux t]: swap the order of two nested loops, the targeted loop
      the immediate inner loop
    params:
      t: ast of the targeted loop
    return: 
      updated ast with swapped loops
 *)
let swap_aux (t : trm) : trm = 
  match Internal.extract_loop t with 
  | Some (loop1, body1) ->
    begin match body1.desc with 
    | Trm_seq[loop2] ->
       begin match Internal.extract_loop loop2 with 
      | Some (loop2, body2) -> loop2 (trm_seq [(loop1 body2)])
      | None -> fail body1.loc "swap_aux: should target a loop with nested loop inside"
      end
    | _ -> fail body1.loc "swap_aux: body of the loop should be a sequence"
    end
    
  | None -> fail t.loc "swap_aux: should target a loop"


let swap : Target.Transfo.local =
  Target.apply_on_path (swap_aux)


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
    let is_step_equal_one = begin match step.desc with 
                            | Trm_val (Val_lit (Lit_int 1)) -> true
                            | _ -> false 
                            end in
    trm_for (i_color) direction start (trm_var nb_colors) step (
      trm_seq [
        trm_for index direction (if is_step_equal_one then trm_var i_color else trm_apps (trm_binop Binop_mul) [trm_var i_color; step]) stop
         (if is_step_equal_one then  trm_var nb_colors else trm_apps (trm_binop Binop_mul) [trm_var nb_colors; step]) body
      ]
    )
  | _ -> fail t.loc "color_aux: only simple loops are supported"


let color (c : var) (i_color : var) : Target.Transfo.local =
    Target.apply_on_path (color_aux c i_color)

(*  [tile_aux divides b i_block t]: tile loop t
      params:
        b: a variable used to represent the block size
        i_block: string used to represent the index used for the new outer loop
        t: ast of the loop going to be tiled
      return:
        updated ast with the tiled loop
*)
let tile_aux (divides : bool) (b : var) (i_block : var) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, direction, start, stop, step, body) ->
     let spec_stop = if not divides then trm_apps (trm_var "min")
                      [ stop;
                        trm_apps (trm_binop Binop_add)[
                          trm_var i_block;
                          trm_apps ~annot:(Some Mutable_var_get)(trm_unop Unop_get) [trm_var b]]]
                      else
                        trm_apps (trm_binop Binop_add)[
                          trm_var i_block;
                          trm_apps ~annot:(Some Mutable_var_get)(trm_unop Unop_get) [trm_var b]] in
     trm_for i_block direction start stop (trm_var b) (
       trm_seq [
         trm_for index direction (trm_var i_block) spec_stop step body])
  | _ -> fail t.loc "tile_aux: only simple loops are supported"


let tile (divides : bool) (b : var)(i_block : var) : Target.Transfo.local =
   Target.apply_on_path (tile_aux divides b i_block)



(* [hoist_aux x_step t]: extract a loop variable inside the loop as an array with size equal
      to (loop_bound - 1), the change all the occurrences of the variable with an array access 
      with index same as the index of the loop
    params:
      x_step: a new_variable name going to appear as the extract variable
      t: ast of the loop 
    return:
      updated ast with the hoisted loop
*)
let hoist_aux (x_step : var) (t : trm) : trm =
  match t.desc with
  | Trm_for (index, direction, start, stop, step, body) ->
    begin match body.desc with
    | Trm_seq tl ->
      (* We assume that the first elment in the body is a variable declaration *)
      let var_decl = List.nth tl 0 in
      let var_name, var_typ = match var_decl.desc with
      | Trm_let (_, (x, tx), _) -> x, tx
      | _ -> fail var_decl.loc "hoist_aux: first loop_body trm should be a variable declaration"
      in
      let remaining_body_trms = List.tl tl in
      let remaining_body_trms = List.map(fun t -> (Internal.change_trm (trm_var var_name) (trm_apps (trm_binop Binop_array_cell_addr) [trm_var x_step; trm_var index] ) t)) remaining_body_trms in
      let var_typ =
      begin match var_typ.typ_desc with
      | Typ_ptr {inner_typ = ty; _} -> ty
      | _ -> fail var_decl.loc "hoist_aux: expected a generated pointer type"
      end
      in
      let new_body = trm_seq ([
        trm_let Var_mutable (var_name, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (var_typ)) (trm_apps (trm_prim (Prim_new var_typ)) [trm_apps (trm_binop Binop_array_cell_addr) [trm_var x_step; trm_var index]])
      ] @ remaining_body_trms) in
      trm_seq ~annot:(Some No_braces)[
        trm_let Var_mutable (x_step, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_array var_typ (Trm stop))) (trm_prim (Prim_new var_typ));
        trm_for index direction start stop step new_body
      ]
    | _ -> fail body.loc "hoist_aux: expected the body of the loop as a sequence"
    end
  | _ -> fail t.loc "hoist_aux: only simple loops are supported"


let hoist (x_step : var) : Target.Transfo.local =
   Target.apply_on_path (hoist_aux x_step)

(* [extract_variable_aux decl_index t] similar to loop hoist *)
let extract_variable_aux (decl_index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_for (index, direction, start, stop, step, body) ->
    begin match body.desc with 
    | Trm_seq tl ->
      let lfront, lback = Tools.split_list_at decl_index tl in
      let var_decl, lback = Tools.split_list_at 1 lback in
      let var_decl = begin match var_decl with 
                     | [vd] -> vd
                     | _ -> fail t.loc "extract_variable_aux: wrong index"
                     end in
      begin match var_decl.desc with 
      | Trm_let (_, (x, tx), _) -> 
        let lback = List.map (
          Internal.change_trm (trm_var x) 
          (trm_apps (trm_binop Binop_array_cell_addr) [trm_var x; trm_var index] )
        ) lback in
        trm_seq ~annot:(Some No_braces) [
          trm_let Var_mutable (x, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_array (get_inner_ptr_type tx) (Trm stop))) (trm_prim (Prim_new (typ_array (get_inner_ptr_type tx) (Trm stop))));
          trm_for index direction start stop step (trm_seq ~annot:body.annot (lfront @ lback))
        ]
      | _ -> fail var_decl.loc "extract_variable_aux: expected the declaration of the variable to be extracted"
      end
    | _ -> fail body.loc "exptract_variable_aux: body of the loop should be a sequence"
    end
  | _ -> fail t.loc "extract_variable_aux: expected a for loop"


let extract_variable (index : int) : Target.Transfo.local =
  Target.apply_on_path(extract_variable_aux index)

(* [split_aux]: split a loop into two loops
    params:
      index: index of the splitting point inside the body of the loop
      t: ast of the loop
    return
      the updated with the splitted loop
 *)
 let split_aux (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_for (loop_index, direction, start, stop, step, body) ->
    begin match body.desc with
    | Trm_seq tl ->
      let first_part, last_part = Tools.split_list_at index tl in
      let first_body = trm_seq first_part in
      let second_body = trm_seq last_part in
      trm_seq ~annot:(Some No_braces) [
        trm_for loop_index direction start stop step first_body;
        trm_for loop_index direction start stop step second_body;
      ]
    | _ -> fail t.loc "split_aux: expected the sequence inside the loop body"
    end
  | _ -> fail t.loc "split_aux: onl simple loops are supported"

 let split (index : int) : Target.Transfo.local=
  Target.apply_on_path (split_aux index)


(* [fusion_aux t]: merge two loops with the same components except the body
    params:
      t: ast of the sequence containing the loops 
    return
      update ast with the merged loops
 *)

let fusion_aux (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    (* Assumption the sequence contains only two trms, the first one is the first loop *)
    let first_loop = List.nth tl 0  in
    (* The second one is the second loop *)
    let second_loop = List.nth tl 1  in

    (* Get the list of trms for the first loop, other parts of the loop are not needed since the assumtion is that the loop
      have the same index, bound and step *)
    let first_loop_trms =
    begin match first_loop.desc with
    | Trm_for (_, _,  _, _, _, body ) ->
      begin match body.desc with
      | Trm_seq tl -> tl
      | _ -> fail t.loc "fusion_aux: expected the first loop body sequence"
      end
    | _ -> fail t.loc "fusion_aux: expected the first for loop"
    end in

    begin match second_loop.desc with
    | Trm_for (index, direction, start, stop, step, body) ->
      (* Extracting the body trms from the second loop *)
      let new_body = begin match body.desc with
      | Trm_seq tl -> trm_seq (first_loop_trms @ tl )
      | _ -> fail t.loc "fusion_aux: expected the second loop body sequence"
      end
      in
      (* The fusioned loop *)
      trm_seq ~annot:t.annot [trm_for index direction start stop step new_body]
    | _ -> fail t.loc "fusion_aux: expected the second loop"
    end
  | _ -> fail t.loc "fusion_aux: expected the sequence which contains the two loops to be merged"

let fusion : Target.Transfo.local =
  Target.apply_on_path (fusion_aux)

(* [grid_enumerate_aux index_and_bounds t]: transform a loop over a grid into ndested loops over each dimension
      of the grid
    params:
      [index_and_bounds]: a list of pairs representing the index and the bound of the loop over each dimesnion 
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
                    trm_seq ([old_loop_index_decl] @ tl)
                   | _ -> fail body.loc "grid_enumerate_aux: the body of the loop should be a sequence"
                   end in 
    Tools.foldi (fun i acc (ind, bnd) ->
      if i = 0 then  trm_for ind direction (trm_lit (Lit_int 0)) (trm_var bnd) (trm_lit (Lit_int 1)) acc
        else  trm_for ind direction (trm_lit (Lit_int 0)) (trm_var bnd) (trm_lit (Lit_int 1)) (trm_seq [acc])
    )new_body (List.rev index_and_bounds)
  | _ -> fail t.loc "grid_enumerate_aux: expected a simple loop"


let grid_enumerate (index_and_bounds : (string * string) list) : Target.Transfo.local =
  Target.apply_on_path (grid_enumerate_aux index_and_bounds)




(* [unroll_aux index t]: extract the body of the loop as a list of list of instructions 
    params:
      index: index of the loop inside the sequence containing the loop 
    return:
      updated ast with the unrolled loop
*)
let unroll_aux (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let loop_to_unroll, lback = Tools.split_list_at 1 lback in 
    let loop_to_unroll = 
      match loop_to_unroll with
      | [lt] -> lt
      | _ -> fail t.loc "unroll_aux: the targeted loop was not matched correctly"
      in
    begin match loop_to_unroll.desc with 
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
      let unrolled_body = begin match body.desc with 
                          | Trm_seq tl1 ->
                            List.fold_left( fun acc i1 -> 
                               let new_index = Internal.change_trm (trm_lit (Lit_int unroll_bound)) (trm_lit (Lit_int i1)) stop in
                               trm_seq ~annot:(Some No_braces) (List.map (Internal.change_trm (trm_var index) new_index) tl1) :: acc
                            ) [] (List.rev unrolled_loop_range)
                          | _ -> fail body.loc "unroll_aux: body of the loop should be a sequence"
                          end in
      trm_seq ~annot:t.annot (lfront @ unrolled_body @ lback)
    | _ -> fail loop_to_unroll.loc "unroll_aux: only simple loops supported"
    end
  | _ -> fail t.loc "unroll_aux: expected the surrounding sequence"


let unroll (index : int) : Target.Transfo.local =
  Target.apply_on_path (unroll_aux index)