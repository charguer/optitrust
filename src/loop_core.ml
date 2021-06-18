open Ast
(* swap_aux: This is an auxiliary function for swap
    params:  
      t: an ast subterm
    return: the updated ast
 *)
 let swap_aux (t : trm) : trm = 
  match t.desc with
  | Trm_for (init1, cond1, step1,body1) ->
      (* Ast_to_text.print_ast ~only_desc:true stdout body1; *)
      begin match body1.desc with
      | Trm_seq [f_loop] ->
        begin match f_loop.desc with
        | Trm_for(init2 ,cond2 ,step2, body2) ->
          trm_for init2 cond2 step2 (trm_seq [trm_for init1 cond1 step1 body2])
        | _ -> fail t.loc "swap_aux: inner_loop was not matched"
        end
      | _ -> fail t.loc "swap_aux; expected inner loop"
      end
  | _ -> fail t.loc "swap_aux; bad loop body"



(* swap: Swap the two loop constructs, the loop should contain as least one inner loop
    params:
      path_to_loop an explicit path toward the loop
      t: ast
    return: 
      the modified ast
*)
let swap : Target.Transfo.local =
  Target.apply_on_path (swap_aux) 


(*  color_aux: This function is an auxiliary function for color
      params:
        c: a variable used to represent the number of colors
        i_color: string used to represent the index used of the new outer loop
        t: an ast subterm
      return: 
        the updated ast
*)

let color_aux (c : var) (i_color : var) (t : trm) : trm =
  match t.desc with 
  | Trm_for (_ , _, _, body) ->
    let index_i = for_loop_index t in
    let loop_size = for_loop_bound t in
    (* let block_size = trm_var c in *)
    let loop_step = for_loop_step t in
    
    let loop ?(top : bool = false) (index : var) (bound : trm) (body : trm) =
      let start = match top with
      | true -> trm_lit(Lit_int 0)
      | false ->
        match loop_step.desc with
        | Trm_val(Val_lit(Lit_int 1)) -> trm_var i_color
        | _ -> trm_apps (trm_binop Binop_mul)
            [
                trm_apps ~annot:(Some Mutable_var_get)
                    (trm_unop Unop_get) [trm_var i_color];
                  loop_step
            ]
        in
        
            trm_for
              (*init *)
              (trm_let ~loc:start.loc Var_mutable (index,typ_ptr (typ_int ())) (trm_apps (trm_prim ~loc:start.loc (Prim_new (typ_int ()))) [start]))
              (* cond *)
              (trm_apps (trm_binop Binop_lt)
                [
                  trm_apps ~annot:(Some Mutable_var_get)
                    (trm_unop Unop_get) [trm_var index];
                    bound
                ]
              )
              (* step *)
              (if top then trm_apps (trm_unop Unop_inc) [trm_var index]
              else  match loop_step.desc with
                | Trm_val(Val_lit(Lit_int 1)) -> trm_set (trm_var index) ~annot:(Some App_and_set)
                  (trm_apps (trm_binop Binop_add)
                    [
                      trm_var index ;
                      trm_var c
                    ])
                | _ ->
                  trm_set (trm_var index) ~annot:(Some App_and_set) (trm_apps (trm_binop Binop_add)
                    [
                      trm_var index;
                      trm_apps (trm_binop Binop_mul)
                           [
                             trm_apps ~annot:(Some Mutable_var_get)
                               (trm_unop Unop_get) [trm_var c];
                             loop_step
                           ]
                    ])
              )
              (* body *)
              body;

        in loop ~top:true ("c" ^ index_i) (trm_var c) (trm_seq [loop ~top:false index_i loop_size body ])

  | _ -> fail t.loc "color_aux: not a for loop, check the path "

(* color: Replace the original loop with two nested loops:
        for (int i_color = 0; i_color < C; i_color++)
          for ([int] i = a+i_color; i < b; i += C) 
      params:
        path_to_loop: explicit path toward the loop
        c: a variable used to represent the number of colors
        i_color: index used for the new outer loop 
        t: ast
      return:
        the modified ast
*)
let color (c : var) (i_color : var) : Target.Transfo.local =
    Target.apply_on_path (color_aux c i_color) 

(*  tile_aux: This function is an auxiliary function for loop
      params:
        b: a variable used to represent the block size
        i_block: string used to represent the index used for the new outer loop
        t: an ast subterm
      return: 
        the updated ast
*)
let tile_aux (b : var) (i_block : var) (t : trm) : trm =
  match t.desc with
  | Trm_for (_, _, _, body) ->
    let index_x = for_loop_index t in
    let loop_size = for_loop_bound t in
    (* let block_size =  trm_var b in *)
    let spec_bound = trm_apps (trm_var "min")
          [
            loop_size;
            trm_apps (trm_binop Binop_add)
            [

              trm_var ("b" ^ index_x);
              trm_apps ~annot:(Some Mutable_var_get)
                      (trm_unop Unop_get) [trm_var b]
            ]
          ]
    in
    let loop ?(top : bool = false) (index : var) (bound : trm) (body : trm) =
        let start = match top with
        | true -> trm_lit(Lit_int 0)
        | false -> trm_var( i_block)
        in
        
        trm_for
          (* init *)
          (trm_let ~loc:start.loc Var_mutable (index,typ_ptr (typ_int ())) (trm_apps (trm_prim ~loc:start.loc (Prim_new (typ_int ()))) [start]))
                
          (* cond *)
          (trm_apps (trm_binop Binop_lt)
           [
             trm_apps ~annot:(Some Mutable_var_get)(trm_unop Unop_get) 
             [trm_var index];
              bound]
          )
          (* step *)
          ( if not top then trm_apps (trm_unop Unop_inc) [trm_var index]
            else 
              trm_set (trm_var index ) ~annot:(Some App_and_set)(trm_apps (trm_binop Binop_add)
                [
                      trm_var index;
                      trm_apps ~annot:(Some Mutable_var_get) (trm_unop Unop_get) [trm_var b]
                ]
              )

          )
          (* body *)
          (body)
        in
        loop ~top:true i_block loop_size (trm_seq [loop ~top:false index_x spec_bound body])
     | _ -> fail t.loc "tile_aux: bad loop body"


(* tile: Replace the original loop with two nested loops 
      params: 
        path_to_loop: an explicit path to the loop
        b: a variable used to represent the block size
        i_block: index used for the new outer loop
        t: ast
      return: 
        updated ast

*)
let tile (b : var)(i_block : var) : Target.Transfo.local = 
   Target.apply_on_path (tile_aux b i_block) 

(*  tile_old_aux: This function is an auxiliary function for tile_old
      params:
        b: a variable used to represent the block size
        i_block: string used to represent the index used for the new outer loop
        t: an ast subterm
      return: 
        the updated ast
*)
let tile_old_aux (t : trm) : trm =
  match t.desc with
  | Trm_for (_ , _, _, body) ->
     begin match body.desc with
     (* look for the declaration of i1 and i2 *)
     | Trm_seq (t_decl1 :: t_decl2 :: tl) ->
        let i = for_loop_index t in
        let i1 = decl_name t_decl1 in
        let i2 = decl_name t_decl2 in
        let block_size =
          let init = decl_init_val t_decl1 in
          match init.desc with
          | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_div)); _},
                      [_; block_size]) ->
             block_size
          | _ ->
             fail t_decl1.loc "tile_old_aux: bad initialisation"
        in
        let loop_size = for_loop_bound t in
        
        let nb_blocks =
          trm_apps (trm_binop Binop_div) [loop_size; block_size]
        in
        (*
          if i is still used in the loop body, add an instruction
          i = i1 * block_size + i2
         *)
        (* TODO: Fix this later *)
        let body =
          if not (is_used_var_in (trm_seq tl) i) then trm_seq tl
          else
            trm_seq
              ((trm_seq ~annot:(Some Mutable_var_get)
                  [
                    (trm_let  Var_mutable (i,typ_ptr (typ_int ())) (trm_apps (trm_prim  (Prim_new (typ_int ()))) [(trm_apps (trm_binop Binop_add)
                         [
                           trm_apps (trm_binop Binop_mul)
                             [
                               trm_apps ~annot:(Some Mutable_var_get)
                                 (trm_unop Unop_get) [trm_var i1];
                               block_size
                             ];
                           trm_apps ~annot:(Some Mutable_var_get)
                             (trm_unop Unop_get) [trm_var i2]
                         ]
                      )]))
                  ]
               ) :: tl)
        in

        
        let loop (index : var) (bound : trm) (body : trm) =
          trm_seq (* ~annot:(Some Delete_instructions) *)
            [
              trm_for
                (* init *)
                (trm_let Var_mutable (index,typ_ptr (typ_int ())) (trm_apps (trm_prim  (Prim_new (typ_int ()))) [trm_lit (Lit_int 0)]))
                (* cond *)
                (trm_apps (trm_binop Binop_lt)
                   [
                     trm_apps ~annot:(Some Mutable_var_get)
                       (trm_unop Unop_get) [trm_var index];
                     bound
                   ])

                (* step *)
                (trm_apps (trm_unop Unop_inc) [trm_var index])
                (* body *)
                body;
            ]
        in
        loop i1 nb_blocks (trm_seq [loop i2 block_size body])
     | _ -> fail t.loc "tile_old__aux: bad loop body"
     end
  | _ -> fail t.loc "tile_old__aux: not a for loop"


(* tile_old: Replace the original loop with two nested loops 
      params: 
        path_to_loop: an explicit path to the loop
        b: a variable used to represent the block size
        i_block: index used for the new outer loop
        t: ast
      return: 
        updated ast

*)
let tile_old : Target.Transfo.local =
   Target.apply_on_path (tile_old_aux) 


(* hoist_aux: This is an auxiliary function for hoist
    params:
      x_step: a new_variable name
      t: an ast subterm
    return: 
      the updated ast
*)
let hoist_aux (x_step : var) (t : trm) : trm =
  match t.desc with 
  | Trm_for (init, cond, step,body) ->
    begin match body.desc with 
    | Trm_seq tl ->
      (* We assume that the first element in the body is a variable *)
      let var_decl = List.nth tl 0 in
      let var_name, var_typ = match var_decl.desc with
      | Trm_let (_,(x, tx),_) -> x, tx
      | _ -> fail t.loc "hoist_aux: first loop body trm should be a variable declaration"
      in
      Tools.printf "%s\n" (Tools.document_to_string (Ast_to_text.print_typ ~only_desc:true var_typ));
      (* Get the loop index *)
      let index = for_loop_index (t) in
      let bound = for_loop_bound (t) in
      let remaining_body_trms = List.tl tl in
      let remaining_body_trms = List.map(fun t -> (Generic_core.change_trm (trm_var var_name) (trm_apps (trm_binop Binop_array_access) [trm_var x_step; trm_var index] ) t)) remaining_body_trms in
      let var_typ = 
      begin match var_typ.typ_desc with 
      | Typ_ptr ty -> ty
      | _ -> fail var_decl.loc "hoist_aux: expected a type pointer"
      end
      in
      (* (trm_apps (trm_prim ~loc (Prim_new tt)) [te]) *)
      
      
      let new_body = trm_seq ([
        trm_let Var_mutable (var_name, typ_ptr (var_typ)) (trm_apps (trm_prim (Prim_new var_typ)) [trm_apps (trm_binop Binop_array_access) [trm_var x_step; trm_var index]])
      ] @ remaining_body_trms) in
      trm_seq ~annot:(Some No_braces) [
        trm_let Var_mutable (x_step, typ_ptr (typ_array var_typ (Trm (bound)))) (trm_prim (Prim_new var_typ));
        trm_for init cond step new_body
      ]
    | _ -> fail t.loc "hoist_aux: expected the sequence inside the body of the loop"
    end
  | _ -> fail t.loc "hoist_aux: the given path does not resolve to a for loop"
    
(* hoist:  Extract a variable from loop
    params:
      path_to_loop: an explicit path to the loop
      x_step: a fresh name for the new_variable
      t: ast
    return:
      the updated ast
 *)
let hoist (x_step : var) : Target.Transfo.local =
   Target.apply_on_path (hoist_aux x_step)


(* split_aux: This is an auxiliary function for split
    params:
      index: int
      t: an ast subterm
    return
      the updated ast
 *)
 let split_aux (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_for (init, cond, step, body) ->
    begin match body.desc with 
    | Trm_seq tl ->
      let first_part, last_part = Tools.split_list_at index tl in
      let first_body = trm_seq first_part in
      let second_body = trm_seq last_part in
      trm_seq ~annot:(Some No_braces) [
        trm_for init cond step first_body;
        trm_for init cond step second_body;
      ]  
    | _ -> fail t.loc "split_aux: expected the sequence inside the loop body"
    end
  | _ -> fail t.loc "split_aux: the given path does not resolve to a for loop"


(* split: Split the loop into two loops, the spliting point is defined as the index of the n-th trm in the loop body
    params:
      path_to_loop: an explicit path to the loop
      index: an index in the range 0 .. N (though in practice only 1 .. N-1 is useful)
      t: ast
    return: 
      the updated ast
 *)

 let split (index : int) : Target.Transfo.local=
  Target.apply_on_path (split_aux index)


(* fusion_aux: This function is an auxiliary function for fusion
    params:
      t: an ast subterm
    return
      the updated ast
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
    | Trm_for(_, _, _, body) ->
      begin match body.desc with 
      | Trm_seq tl -> tl
      | _ -> fail t.loc "fusion_aux: expected the first loop body sequence"
      end
    | _ -> fail t.loc "fusion_aux: expected the first for loop"
    end in

    begin match second_loop.desc with 
    | Trm_for (init, cond, step, body) ->
      (* Extracting the body trms from the second loop *)
      let new_body = begin match body.desc with 
      | Trm_seq tl -> trm_seq (first_loop_trms @ tl )
      | _ -> fail t.loc "fusion_aux: expected the second loop body sequence"
      end
      in
      (* The fusioned loop *)
      trm_seq ~annot:t.annot [trm_for init cond step new_body]
    | _ -> fail t.loc "fusion_aux: expected the second loop"
    end
  | _ -> fail t.loc "fusion_aux: expected the sequence which contains the two loops to be merged"


(* fusion: Merge to loops with  the same range
    params:
      path_to_seq: Path to the sequence which contains the two loops
      t: ast
    returns
      the updated ast
*)
let fusion : Target.Transfo.local =
  Target.apply_on_path(fusion_aux)

  