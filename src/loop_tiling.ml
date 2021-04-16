open Ast
open Paths
open Transformations
open Ast_to_c

(*
  transform a pre-tiled loop of the form
  optional_label:
  for i = 0; i < N; i++
    int i1 = i / block_size
    int i2 = i % block_size
    body
  into a loop of the form
  optional_label:
  for i1 = 0; i1 < N / block_size; i1++
    for i2 = 0; i2 < block_size; i2++
      i = i1 * block_size + i2 // only if i is used in body
      body
 *)

let rec tile_loop_aux (clog : out_channel) (t : trm) : trm =
  match t.desc with
  (* the loop might be labelled: keep the label *)
  | Trm_labelled (l, t_loop) ->
     trm_labelled l (tile_loop_aux clog t_loop)
  (*
    if the loop declares its own index, a seq with a delete instruction occurs
    in this case, put the delete instructions at the end of the inner loop if
    the index is still used
   *)
  | Trm_seq [t_loop; t_del] when t.annot = Some Delete_instructions ->
     let t_tiled = tile_loop_aux clog t_loop in
     (* tiled loops are expected to declare their index *)
     begin match t_tiled.desc with
     | Trm_seq [{desc = Trm_for (init1, cond1, step1,
                                 {desc = Trm_seq [body1]; _}); _}; t_del1]
          when t_tiled.annot = Some Delete_instructions ->
        begin match body1.desc with
        | Trm_seq [{desc = Trm_for (init2, cond2, step2, body); _}; t_del2]
             when body1.annot = Some Delete_instructions ->
           (* if the index is used in body, then add delete instruction *)
           let i = deleted_var t_del in
           if not (is_used_var_in body i) then t_tiled
           else
             trm_seq ~annot:(Some Delete_instructions)
               [
                 trm_for init1 cond1 step1
                   (trm_seq
                      [trm_seq ~annot:(Some Delete_instructions)
                         [
                           trm_for init2 cond2 step2
                             (trm_seq ~annot:(Some Delete_instructions)
                                [body; t_del]);
                           t_del2
                         ]
                      ]
                   );
                 t_del1
               ]
        | _ -> fail body1.loc "tile_loop_aux: expected inner loop"
        end
     | _ -> fail t_tiled.loc "tile_loop_aux: expected outer loop"
     end
  (* otherwise, just tile *)
  | Trm_for (init, cond, step, body) ->
     let log : string =
       let loc : string =
         match body.loc with
         | None -> ""
         | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
       in
       Printf.sprintf
         ("  - for (%s; %s; %s) is of the form\n" ^^
          "      for ([int] i = 0; i < N; i++)\n" ^^
          "  - expression\n%s\n" ^^
          "    %sis of the form\n" ^^
          "      {\n" ^^
          "        int i1 = i / block_size\n" ^^
          "        int i2 = i %% block_size\n" ^^
          "        body\n" ^^
          "      }\n"
         )
         (ast_to_string init) (ast_to_string cond) (ast_to_string step)
         (ast_to_string body) loc
     in
     write_log clog log;
     begin match body.desc with
     (* look for the declaration of i1 and i2 *)
     | Trm_seq ({desc = Trm_seq (t_decl1 :: t_decl2 :: tl); _} :: t_del_l)
          when body.annot = Some Delete_instructions ->
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
             fail t_decl1.loc "tile_loop_aux: bad initialisation"
        in
        let loop_size = for_loop_bound t in
        let log : string =
          Printf.sprintf "  - %s is divisible by %s\n" (ast_to_string loop_size)
            (ast_to_string block_size)
        in
        write_log clog log;
        let nb_blocks =
          trm_apps (trm_binop Binop_div) [loop_size; block_size]
        in
        (*
          if i is still used in the loop body, add an instruction
          i = i1 * block_size + i2
         *)
        let body =
          if not (is_used_var_in (trm_seq tl) i) then trm_seq tl
          else
            trm_seq
              ((trm_seq ~annot:(Some Heap_allocated)
                  [
                    trm_decl (Def_var ((i, typ_ptr (typ_int ())),
                                       trm_prim (Prim_new (typ_int ()))));
                    trm_set ~annot:(Some Initialisation_instruction)
                      (trm_var i)
                      (trm_apps (trm_binop Binop_add)
                         [
                           trm_apps (trm_binop Binop_mul)
                             [
                               trm_apps ~annot:(Some Heap_allocated)
                                 (trm_unop Unop_get) [trm_var i1];
                               block_size
                             ];
                           trm_apps ~annot:(Some Heap_allocated)
                             (trm_unop Unop_get) [trm_var i2]
                         ]
                      )
                  ]
               ) :: tl)
        in
        (*
          look for the deletion of i1 and i2:
          if only i1 and i2 are deleted, the loop body is correct
          otherwise, add the remaining delete instructions
         *)
        let body =
          match List.rev t_del_l with
          | [_; _] -> body
          | _ :: _ :: t_del_l' ->
             trm_seq ~annot:(Some Delete_instructions)
               (body :: List.rev t_del_l')
          | _ ->
             fail t.loc "tile_loop_aux: bad delete instructions"
        in
        let loop (index : var) (bound : trm) (body : trm) =
          trm_seq ~annot:(Some Delete_instructions)
            [
              trm_for
                (* init *)
                (trm_seq ~annot:(Some Heap_allocated)
                   [
                     trm_decl (Def_var ((index, typ_ptr (typ_int ())),
                                        trm_prim (Prim_new (typ_int ()))));
                     trm_set ~annot:(Some Initialisation_instruction)
                       (trm_var index) (trm_lit (Lit_int 0))
                   ]
                )
                (* cond *)
                (trm_apps (trm_binop Binop_lt)
                   [
                     trm_apps ~annot:(Some Heap_allocated)
                       (trm_unop Unop_get) [trm_var index];
                     bound
                   ]
                )
                (* step *)
                (trm_apps (trm_unop Unop_inc) [trm_var index])
                (* body *)
                body;
              trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ()))
                (trm_unop (Unop_delete false)) [trm_var index]
            ]
        in
        loop i1 nb_blocks (trm_seq [loop i2 block_size body])
     | _ -> fail t.loc "tile_loop_aux: bad loop body"
     end
  | _ -> fail t.loc "tile_loop_aux: not a for loop"

let tile_loop_aux (clog : out_channel) (t : trm) : trm =
  let log : string =
    let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in
    Printf.sprintf
      ("  - expression\n%s\n" ^^
       "    %sis a (labelled) loop\n"
      )
      (ast_to_string t) loc
  in
  write_log clog log;
  tile_loop_aux clog t

let tile_loop (clog : out_channel) (pl : path list)
  (t : trm) : trm =
  let p = List.flatten pl in
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_path p t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "tile_loop: no matching subterm\n";
     t
  | _ ->
     List.fold_left
       (fun t dl ->
         apply_local_transformation (tile_loop_aux clog) t dl)
       t
       epl
