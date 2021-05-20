open Ast
open Ast_to_c
open Target
open Path_constructors
open Transformations
open Tools

 let rec loop_coloring_aux (clog : out_channel) (c : var) (new_var : var)(t : trm) : trm =
    match t.desc with
    (* The loop might be labelled, so keep the label *)
    | Trm_labelled (l, t_loop) ->
      trm_labelled l (loop_coloring_aux clog c new_var t_loop)

    | Trm_seq [t_loop; t_del] (* when t.annot = Some Delete_instructions *) ->
     let t_transformed = loop_coloring_aux clog c new_var t_loop in
     (* transformed loops are expected to declare their index *)
     begin match t_transformed.desc with
     | Trm_seq [{desc = Trm_for (init1, cond1, step1,
                                 {desc = Trm_seq [body1]; _}); _}; t_del1]
         (*  when t_transformed.annot = Some Delete_instructions *) ->
        begin match body1.desc with
        | Trm_seq [{desc = Trm_for (init2, cond2, step2, body); _}; t_del2]
             (* when body1.annot = Some Delete_instructions *) ->
           (* if the index is used in body, then add delete instruction *)
           let i = deleted_var t_del in
           if not (is_used_var_in body i) then t_transformed
           else
             trm_seq (* ~annot:(Some Delete_instructions) *)
               [
                 trm_for init1 cond1 step1
                   (trm_seq
                      [trm_seq (* ~annot:(Some Delete_instructions) *)
                         [
                           trm_for init2 cond2 step2
                             (trm_seq (* ~annot:(Some Delete_instructions) *)
                                [body; t_del]);
                           t_del2
                         ]
                      ]
                   );
                 t_del1
               ]
        | _ -> fail body1.loc "loop_coloring_aux: expected inner loop"
        end
     | _ -> fail t_transformed.loc "loop_coloring_aux: expected outer loop"
     end
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
          "        body\n" ^^
          "      }\n"
         )
         (ast_to_string init) (ast_to_string cond) (ast_to_string step)
         (ast_to_string body) loc
      in
      write_log clog log;
      let index_i = for_loop_index t in
      let loop_size = for_loop_bound t in
      let block_size = trm_var c in
      let loop_step = for_loop_step t in
      let log : string =
        Printf.sprintf " - %s is divisible by %S\n" (ast_to_string loop_size) (ast_to_string block_size)
      in
      write_log clog log;

      let loop ?(top : bool = false) (index : var) (bound : trm) (body : trm) =


        let start = match top with
        | true -> trm_lit(Lit_int 0)
        | false ->
          match loop_step.desc with
          | Trm_val(Val_lit(Lit_int 1)) -> trm_var new_var
          | _ -> trm_apps (trm_binop Binop_mul)
              [
                  trm_apps ~annot:(Some Heap_allocated)
                      (trm_unop Unop_get) [trm_var new_var];
                    loop_step

              ]

          in
          trm_seq (* ~annot:(Some Delete_instructions) *)
            [
              trm_for
                (*init *)
                (trm_seq ~annot:(Some Heap_allocated)
                  [
                    trm_decl (Def_var ((index, typ_ptr (typ_int())), trm_prim (Prim_new (typ_int ()))));
                    trm_set ~annot:(Some Initialisation_instruction)
                    (trm_var index) start
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
                               trm_apps ~annot:(Some Heap_allocated)
                                 (trm_unop Unop_get) [trm_var c];
                               loop_step
                             ]

                      ])
                )




                (* body *)
                body;
                trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ()))
                  (trm_unop (Unop_delete false)) [trm_var index]

            ]
          in loop ~top:true new_var (trm_var c) (trm_seq [loop ~top:false index_i loop_size body ])


  | _ -> fail t.loc "loop_coloring_aux: not a for loop, check the path "


let loop_coloring_aux (clog : out_channel)(c : var)(new_var : var) (t : trm) : trm =
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
    loop_coloring_aux clog c new_var t


let loop_coloring (clog : out_channel) (tr : target) (c : var)(new_var : var)(t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "loop_coloring: no matching subterm\n";
     t
  | _ ->
     List.fold_left
       (fun t dl ->
         apply_local_transformation (loop_coloring_aux clog c new_var) t dl)
       t
       epl

let rec loop_tile_aux (clog : out_channel)(b : var)(new_var : var) (t : trm) : trm =
  match t.desc with
  (* the loop might be labelled: keep the label *)
  | Trm_labelled (l, t_loop) ->
     trm_labelled l (loop_tile_aux clog b new_var t_loop)
  (*
    if the loop declares its own index, a seq with a delete instruction occurs
    in this case, put the delete instructions at the end of the inner loop if
    the index is still used
   *)
  | Trm_seq [t_loop; t_del] (* when t.annot = Some Delete_instructions *) ->
     let t_tiled = loop_tile_aux clog b new_var t_loop in
     (* tiled loops are expected to declare their index *)
     begin match t_tiled.desc with
     | Trm_seq [{desc = Trm_for (init1, cond1, step1,
                                 {desc = Trm_seq [body1]; _}); _}; t_del1]
          (* when t_tiled.annot = Some Delete_instructions *) ->
        begin match body1.desc with
        | Trm_seq [{desc = Trm_for (init2, cond2, step2, body); _}; t_del2]
             (* when body1.annot = Some Delete_instructions *) ->
           (* if the index is used in body, then add delete instruction *)
           let i = deleted_var t_del in
           if not (is_used_var_in body i) then t_tiled
           else
             trm_seq (* ~annot:(Some Delete_instructions) *)
               [
                 trm_for init1 cond1 step1
                   (trm_seq
                      [trm_seq (* ~annot:(Some Delete_instructions) *)
                         [
                           trm_for init2 cond2 step2
                             (trm_seq (* ~annot:(Some Delete_instructions) *)
                                [body; t_del]);
                           t_del2
                         ]
                      ]
                   );
                 t_del1
               ]
        | _ -> fail body1.loc "loop_tile_aux: expected inner loop"
        end
     | _ -> fail t_tiled.loc "loop_tile_aux: expected outer loop"
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
          "      for ([int] x = 0; x < X; x++)\n" ^^
          "  - expression\n%s\n" ^^
          "    %sis of the form\n" ^^
          "      {\n" ^^
          "        body\n" ^^
          "      }\n"
         )
         (ast_to_string init) (ast_to_string cond) (ast_to_string step)
         (ast_to_string body) loc
    in
    write_log clog log;
    let index_x = for_loop_index t in
    let loop_size = for_loop_bound t in
    let block_size =  trm_var b in
    let spec_bound = trm_apps (trm_var "min")
          [
            loop_size;
            trm_apps (trm_binop Binop_add)
            [

              trm_var ("b" ^ index_x);
              trm_apps ~annot:(Some Heap_allocated)
                      (trm_unop Unop_get) [trm_var b]
            ]
          ]
    in
    let log : string =
      Printf.sprintf "   -%s is divisible by %S\n" (ast_to_string loop_size) (ast_to_string block_size)
    in
    write_log clog log;

    let loop ?(top : bool = false) (index : var) (bound : trm) (body : trm) =
        let start = match top with
        | true -> trm_lit(Lit_int 0)
        | false -> trm_var( new_var)
        in
        trm_seq (* ~annot:(Some Delete_instructions) *)
            [
              trm_for
                (* init *)
                (trm_seq ~annot:(Some Heap_allocated)
                   [
                     trm_decl (Def_var ((index, typ_ptr (typ_int ())),
                                        trm_prim (Prim_new (typ_int ()))));
                     trm_set ~annot:(Some Initialisation_instruction)
                       (trm_var index) start
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
                (if not top then trm_apps (trm_unop Unop_inc) [trm_var index]
                else trm_set (trm_var index ) ~annot:(Some App_and_set)(trm_apps (trm_binop Binop_add)
                    [
                      trm_var index;
                      trm_apps ~annot:(Some Heap_allocated)
                      (trm_unop Unop_get) [trm_var b]

                    ]
                )

                )
                (* body *)
                body;
              trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ()))
                (trm_unop (Unop_delete false)) [trm_var index]
            ]
        in
        loop ~top:true new_var loop_size (trm_seq [loop ~top:false index_x spec_bound body])
     | _ -> fail t.loc "loop_tile_aux: bad loop body"




let loop_tile_aux (clog : out_channel)(b : var)(new_var : var) (t : trm) : trm =
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
  loop_tile_aux clog b new_var t

let loop_tile (clog : out_channel) (tr : target)(tile_width : var)(new_var : var)(t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "loop_tile: no matching subterm\n";
     t
  | _ ->
     List.fold_left
       (fun t dl ->
         apply_local_transformation (loop_tile_aux clog tile_width new_var) t dl)
       t
       epl

let rec loop_swap_aux (clog : out_channel) (t : trm) : trm =
  match t.desc with
  (* the loop might be labelled: kepp the label *)
  | Trm_labelled (l, t_loop) ->
    trm_labelled l (loop_swap_aux clog t_loop)
  | Trm_seq [t_loop; t_del] (* when t.annot = Some Delete_instructions *) ->
     let t_swaped = loop_swap_aux clog t_loop in
     (* swaped loops are expected to declare their index *)
     begin match t_swaped.desc with
     | Trm_seq [{desc = Trm_for (init1, cond1, step1,
                                 {desc = Trm_seq [body1]; _}); _}; t_del1]
          (* when t_swaped.annot = Some Delete_instructions *) ->
        begin match body1.desc with

        | Trm_seq [{desc = Trm_for (init2, cond2, step2, body); _}; t_del2]
             (* when body1.annot = Some Delete_instructions *) ->
           (* if the index is used in body, then add delete instruction *)
           let i = deleted_var t_del in
           if not (is_used_var_in body i) then t_swaped
           else
             trm_seq (* ~annot:(Some Delete_instructions) *)
               [
                 trm_for init1 cond1 step1
                   (trm_seq
                      [trm_seq (* ~annot:(Some Delete_instructions) *)
                         [
                           trm_for init2 cond2 step2
                             (trm_seq (* ~annot:(Some Delete_instructions) *)
                                [body; t_del]);
                           t_del2
                         ]
                      ]
                   );
                 t_del1
               ]
        | _ -> fail body1.loc "loop_swap_aux: expected inner loop"
        end
     | _ -> fail t_swaped.loc "loop_swap_aux: expected outer loop"
     end
  (* otherwise, just swap  *)
  | Trm_for (init1, cond1, step1,body1) ->
    let log : string =
      let loc : string =
        match body1.loc with
        | None -> ""
        | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
      in
      Printf.sprintf
         ("  - for (%s; %s; %s) is of the form\n" ^^
          "      for ([int] x = 0; x < X; x++)\n" ^^
          "  - expression\n%s\n" ^^
          "    %sis of the form\n" ^^
          "      {\n" ^^
          "        body\n" ^^
          "      }\n"
         )
         (ast_to_string init1) (ast_to_string cond1) (ast_to_string step1)
         (ast_to_string body1) loc
      in
      write_log clog log;

      begin match body1.desc with

      | Trm_seq ({desc = Trm_seq(f_loop :: _);_} :: _) ->
        begin match f_loop.desc with
        | Trm_for(init2,cond2,step2,body2) ->
          let log : string =
            let loc : string =
            match body1.loc with
            | None -> ""
            | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
          in
          Printf.sprintf
          ("Inner looop " ^^
           "  - for (%s; %s; %s) is of the form\n" ^^
           "      for ([int] x = 0; x < X; x++)\n" ^^
           "  - expression\n%s\n" ^^
           "    %sis of the form\n" ^^
           "      {\n" ^^
           "        body\n" ^^
           "      }\n"
          )
          (ast_to_string init2) (ast_to_string cond2) (ast_to_string step2)
          (ast_to_string body2) loc
          in
          write_log clog log;
          let index1 = for_loop_index t in
          let loop_size1 = for_loop_bound t in
          let index_init1 = for_loop_init t in
          let index2 = for_loop_index f_loop in
          let loop_size2 = for_loop_bound f_loop in
          let index_init2 = for_loop_init f_loop in

          let loop (index : var) (init : trm) (step : trm) (bound : trm) (body : trm) =
          trm_seq (* ~annot:(Some Delete_instructions) *)
            [
              trm_for
                (* init *)
                (trm_seq ~annot:(Some Heap_allocated)
                   [
                     trm_decl (Def_var ((index, typ_ptr (typ_int ())),
                                        trm_prim (Prim_new (typ_int ()))));
                     trm_set ~annot:(Some Initialisation_instruction)
                       (trm_var index) (init)
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
                (step)
                (* body *)
                body;
              trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ()))
                (trm_unop (Unop_delete false)) [trm_var index]
            ]
        in
        loop index2 index_init2 step2 loop_size2 (trm_seq [loop index1 index_init1 step1 loop_size1 body2])
        | _ -> fail t.loc "loop_swap_aux: inner_loop was not matched"
        end
      | _ -> fail t.loc "loop_swap_aux; expected inner loop"
      end


  | _ -> fail t.loc "loop_swap_aux; bad loop body"



let loop_swap_aux (clog : out_channel) (t : trm) : trm =
  let log : string =
    let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in
    Printf.sprintf
      (" - expression \n%s\n" ^^
       "  %sis a (labelled) loop\n"
      )
      (ast_to_string t) loc
  in
  write_log clog log;
  loop_swap_aux clog t


let loop_swap (clog : out_channel) (tr : target)(t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "loop_swap: no matching subterm\n";
     t
  | _ ->
     List.fold_left
       (fun t dl ->
         apply_local_transformation (loop_swap_aux clog) t dl)
       t
       epl
(* get_loop_nest_indices -- currently omiting the last one

*)
(* for a { for b {} {  for j {}   ;  for k {} } } -- >  a::b::[]
  the function should check that it is a loop nest :
      aux t =
         if t is a for (i , body) then  i::(aux body)
         if t is a seq[(for(..)) as t1] and nothing else then  aux t1
         else []

*)
let rec get_loop_nest_indices (t : trm) : 'a list =
    match t.desc with
    | Trm_labelled (_, t_loop) -> get_loop_nest_indices t_loop
    | Trm_seq [t_loop;_] -> get_loop_nest_indices t_loop
    | Trm_for (_,_,_,body) ->
      let loop_index = for_loop_index t in
      begin match body.desc with
      | Trm_seq ({desc = Trm_seq (f_loop :: _);_} :: _) ->
        loop_index :: get_loop_nest_indices f_loop
      | _ -> loop_index :: []
      end
    | _ -> []


let move_loop_before_aux (clog : out_channel) (loop_index : var) (t : trm) : trm =
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
      (* Get the path from the outer loop to the one we want to swap with


      let path_list = List.rev (get_path clog t) in

        *)
      let path_list = List.rev (get_loop_nest_indices t)  in
      (* do a list rev at the end of get_loop_nest_indices
         let rec chop_list_before x xs =
            | [] -> error "did not find x"
            | y::tl -> if y = x then [] else y:: chop_list_before x tl      *)
      let rec clean_path (xl : 'a list) : 'a list = match xl with
        | [] -> []
        | hd :: tl ->
          if hd = loop_index then tl
          else clean_path tl
      in
      let _check_last = List.mem loop_index path_list in
      (*
      let path_list = if not check_last then path_list
          else clean_path path_list
      *)
      let path_list = clean_path path_list
      in
      (* List.fold_right (fun i acc  -> loop swap t i) path_list acc t
         --checkout the documentation of fold_right *)
      let rec multi_swap (xl : 'a list) (t : trm) : trm = match xl with
      | [] -> t
      | hd :: tl ->
        let t = loop_swap clog [cFor hd] t in
        multi_swap tl t
     in
     multi_swap path_list t

let move_loop_before (clog : out_channel) (tr : target)(loop_index : var) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
    print_info t.loc "move_loop_before: no matching subterm";
    t
  | _ ->
    List.fold_left
      (fun t dl ->
        apply_local_transformation (move_loop_before_aux clog loop_index) t dl)
      t
      epl

let move_loop_after_aux (clog : out_channel) (loop_index : var) (t : trm) : trm =
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
    let path_list = get_loop_nest_indices t in
    let rec clean_path (xl : 'a list) : 'a list = match xl with
      | [] -> []
      | hd :: tl ->
        if hd = loop_index then tl
        else clean_path tl
      in
    let l_index = List.hd path_list in
    let _check_last = List.mem loop_index path_list in
    let path_list = if false then path_list
      else clean_path (List.rev path_list)
      in
    let path_length = List.length path_list in
    (*if (path_list = []) then error ---try to check the error in case  move_before "i" "i" *)
    (* List.fold_left (fun _i acc -> swap l_index) (List.tl path_list) *)
    let rec multi_swap (count : int) (t : trm) : trm = match count with
      | 0 ->  t
      | _ -> let pl = [cFor l_index ] in
           let t = loop_swap clog pl t in
           multi_swap (count-1) t
      in
    multi_swap path_length t

let move_loop_after (clog : out_channel) (tr : target)(loop_index : var) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
    print_info t.loc "move_loop_before: no matching subterm";
    t
  | _ ->
    List.fold_left
      (fun t dl ->
        apply_local_transformation (move_loop_after_aux clog loop_index) t dl)
      t
      epl

let move_loop (clog : out_channel)  ?(move_before : string = "") ?(move_after : string = "") (loop_index : string) (t : trm) : trm =
  let log : string =
      let loc : string =
        match t.loc with
        | None -> ""
        | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
      in Printf.sprintf
          ("  - expression\n%s\n" ^^
          "    %sis a struct type\n"
          )
      (ast_to_string t) loc
    in
  write_log clog log;
  match move_before, move_after with
  | "",_ -> move_loop_after clog [cFor loop_index] move_after t
  | _,"" -> move_loop_before clog [cFor move_before] loop_index t
  | _,_ -> fail t.loc "move_loop: only one of move_before or move_after should be specified"


 (*
  extract a variable from a loop:
  - before:
    optional_label:
    for i = 0 to N / for i = N to 0
      var x
      body(x)
  - after:
    result_label:
    {
      var x[N+1]
      optional_label:
      for i = 0 to N / for i = N to 0
        body(x[i])
    }
  assumptions:
    + no initialisation for x in its declaration
    + x is heap allocated and deleted last in the loop
    + the path points to the loop
 *)

let rec split_list_at (n : int) (al : 'a list) : 'a list * ('a list) =
  if n <= 0 then ([], al)
  else
    match al with
    | [] -> failwith "split_list_at: not enough elements"
    | a :: al ->
       let (al, al') = split_list_at (n - 1) al in
       (a :: al, al')

(*
  extract nb_vars var declarations from the loop
  they are expected to be heap allocated and at the beginning of the loop body
 *)
let extract_vars_from_loop (clog : out_channel) (nb_vars : int)
  (loop_labels : string list) (t : trm) : trm =
  match t.desc with
  | Trm_for (init, cond, step, body) ->
     let log : string =
       let loc : string =
         match body.loc with
         | None -> ""
         | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
       in
       Printf.sprintf
         ("  - for (%s; %s; %s) is of the form\n" ^^
          "      for ([int] i = 0; i < N; i++) or " ^^
            "for ([int] i = N; i > 0; i--)\n" ^^
          "  - expression\n%s\n" ^^
          "    %sstarts with %d variable declaration(s) where each declared " ^^
            "variable:\n" ^^
          "      + is not initialised in its declaration\n" ^^
          "      + is not a const variable\n"
         )
         (ast_to_string init) (ast_to_string cond) (ast_to_string step)
         (ast_to_string body) loc nb_vars
     in
     write_log clog log;
     begin match body.desc with
     | Trm_seq tl (* when body.annot = Some Delete_instructions *) ->
        (* the variables are expected to be deleted last *)
        let (var_del_l, tl) =
          let (var_del_l, tl) = split_list_at nb_vars (List.rev tl) in
          (List.rev var_del_l, List.rev tl)
        in
        begin match tl with
        | {desc = Trm_seq tl'; _} :: tl'' ->
           let (var_decl_l, tl') = split_list_at nb_vars tl' in
           let n = for_loop_nb_iter t in
           let change_decl (t_decl : trm) : trm =
              let x = decl_name t_decl in
              let tx =
                let tx = var_decl_type t_decl in
                match tx.ty_desc with
                | Typ_ptr tx' -> tx'
                | _ ->
                   fail t_decl.loc
                     "extract_vars_from_loop: expected heap allocated variable"
              in
              let tx' = typ_array tx (Trm n) in
              trm_seq ~annot:(Some Heap_allocated)
                [
                  trm_decl (Def_var ((x, typ_ptr tx'),
                                     trm_prim (Prim_new tx')))
                ]
           in
           let var_decl_l = List.map change_decl var_decl_l in
           let i = for_loop_index t in
           let change_body (tl' : trm list) (t_decl : trm) : trm list =
              let x = decl_name t_decl in
              let x_i =
                trm_apps (trm_binop Binop_array_access) [trm_var x; trm_var i]
              in
              let rec x_to_xi (t' : trm) : trm =
                (*
                  since x is heap allocated, we either have x alone or get x
                  in both cases: replace x with array_access x i
                  for get x: also replace the annotation with Access
                 *)
                match t'.desc with
                | Trm_var y when y = x -> x_i
                | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _},
                            [{desc = Trm_var y; _}])
                     when t'.annot = Some Heap_allocated && y = x ->
                   trm_apps ~annot:(Some Access) (trm_unop Unop_get) [x_i]
                | _ -> trm_map x_to_xi t'
              in
              List.map x_to_xi tl'
           in
           let tl' = List.fold_left change_body tl' var_decl_l in
           let body' =
             match tl'' with
             | [] -> trm_seq tl'
             | _ ->
                trm_seq (* ~annot:(Some Delete_instructions) *)
                  (trm_seq tl' :: tl'')
           in
           (* label the loop if required *)
           let t_loop =
             let t_loop = trm_for init cond step body' in
             match loop_labels with
             | [] -> t_loop
             | _ ->
                List.fold_left (fun t l -> trm_labelled l t) t_loop
                  loop_labels
           in
           trm_seq (* ~annot:(Some Delete_instructions) *)
             ((trm_seq (var_decl_l ++ [t_loop])) :: var_del_l)
        | _ -> fail t.loc "extract_vars_from_loop: bad body"
        end
     | _ -> fail t.loc "extract_vars_from_loop: bad delete instructions"
     end
  | _ -> fail t.loc "extract_vars_from_loop: not a for loop"

let nb_decl_vars (t : trm) : int =
  let rec aux (tl : trm list) : int =
    match tl with
    | [] -> 0
    | t :: tl ->
       begin match t.desc with
       | Trm_decl (Def_var _) -> (aux tl) + 1
       | Trm_seq _ when t.annot = Some Heap_allocated -> (aux tl) + 1
       | _ -> 0
       end
  in
  match t.desc with
  | Trm_for (_, _, _, body) ->
     begin match body.desc with
     | Trm_seq ({desc = Trm_seq tl; _} :: _)
         (*  when body.annot = Some Delete_instructions *) ->
        aux tl
     | Trm_seq tl -> aux tl
     | _ -> fail body.loc "nb_decl_vars: bad loop body"
     end
  | _ -> fail t.loc "nb_decl_vars: expected a for loop"

let rec extract_loop_vars_aux (clog : out_channel) ?(only_one : bool = false)
  ?(loop_labels : string list = []) (result_label : string) (t : trm) : trm =
  match t.desc with
  (*
    if the loop declares its own index, a seq with a delete instruction occurs
    in this case, put the delete instruction back on the loop
   *)
  | Trm_seq [t_loop; t_del_index] (* when t.annot = Some Delete_instructions *) ->
     let nb_vars = if only_one then 1 else nb_decl_vars t_loop in
     if nb_vars = 0 then
       trm_labelled result_label
         (List.fold_left (fun t l -> trm_labelled l t) t_loop loop_labels)
     else
       let t' = extract_vars_from_loop clog nb_vars loop_labels t_loop in
       begin match t'.desc with
       | Trm_seq ({desc = Trm_seq tl; _} :: var_del_l)
            (* when t'.annot = Some Delete_instructions *) ->
          let (var_decl_l, t_loop') =
            let (var_decl_l, tl) = split_list_at nb_vars tl in
            match tl with
            | [t_loop] -> (var_decl_l, t_loop)
            | _ -> fail t'.loc "extract_loop_vars_aux: expected a loop"
          in
          (* the loop might be labelled *)
          let rec add_del (t_loop : trm) : trm =
            match t_loop.desc with
            | Trm_labelled (l, t_loop) -> trm_labelled l (add_del t_loop)
            | _ ->
               trm_seq (* ~annot:(Some Delete_instructions) *) [t_loop; t_del_index]
          in
          let t_loop'' = add_del t_loop' in
          trm_labelled result_label
            (trm_seq (* ~annot:(Some Delete_instructions) *)
               ((trm_seq (var_decl_l ++ [t_loop''])) :: var_del_l))
     | _ -> fail t.loc "extract_loop_vars_aux: bad loop var extraction"
     end
  (* otherwise, only extract *)
  | Trm_for _ ->
     let nb_vars = if only_one then 1 else nb_decl_vars t in
     if nb_vars = 0 then
       trm_labelled result_label
         (List.fold_left (fun t l -> trm_labelled l t) t loop_labels)
     else
       trm_labelled result_label
         (extract_vars_from_loop clog nb_vars loop_labels t)
  (* the loop might be labelled *)
  | Trm_labelled (l, t) ->
     extract_loop_vars_aux clog ~loop_labels:(l :: loop_labels) result_label t
  | _ -> fail t.loc "extract_loop_vars_aux: not a for loop"

let extract_loop_vars_aux (clog : out_channel) ?(only_one : bool = false)
  ?(loop_labels : string list = []) (result_label : string) (t : trm) : trm =
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
  extract_loop_vars_aux clog ~only_one ~loop_labels result_label t

let extract_loop_var (clog : out_channel) (result_label : string)
  (tr : target) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "extract_loop_var: no matching subterm for path %s\n"
       (target_to_string tr);
     t
  | [dl] ->
     apply_local_transformation
       (extract_loop_vars_aux clog ~only_one:true result_label) t dl
  | _ ->
     (*
       folding works since no path in epl is the prefix of a subsequent path
      *)
     foldi
       (fun i ->
         apply_local_transformation
           (extract_loop_vars_aux clog ~only_one:true
              (result_label ^ "_" ^ string_of_int i))
       )
       t
       epl

(* extract all possible vars *)
let extract_loop_vars (clog : out_channel) (result_label : string)
  (tr : target) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "extract_loop_vars: no matching subterm for path %s\n"
       (target_to_string tr);
     t
  | [dl] ->
     apply_local_transformation (extract_loop_vars_aux clog result_label) t dl
  | _ ->
     (*
       folding works since no path in epl is the prefix of a subsequent path
      *)
     foldi
       (fun i ->
         apply_local_transformation
           (extract_loop_vars_aux clog (result_label ^ "_" ^ string_of_int i))
       )
       t
       epl

(*
  split the for loop pointed by pl in t
  assumption: the loop is of the form
  optional_label:
  for i = 0 to N / for i = N to 0
    {block1}
    {block2}
  where block1 and block2 are independent
  result:
    result_label:{
      loop1_label:
      for i = 0 to N / for i = N to 0
        block1
      loop2_label:
      for i = 0 to N / for i = N to 0
        block2
    }
 *)

let rec split_loop_nodep_aux (clog : out_channel) (result_label : string)
  (loop1_label : string) (loop2_label : string) (t : trm) : trm =
  match t.desc with
  (*
    if the loop declares its own index, a seq with a delete instruction occurs
    in this case, duplicate the delete instruction for the two resulting loops
   *)
  | Trm_seq [t_loop; t_del] (* when t.annot = Some Delete_instructions *) ->
     let t' =
       split_loop_nodep_aux clog result_label loop1_label loop2_label t_loop
     in
     begin match t'.desc with
     | Trm_labelled (l, {desc = Trm_seq [t_loop1; t_loop2]; _}) ->
        let add_del_instr (t_loop : trm) : trm =
          match t_loop.desc with
          | Trm_labelled (l', t_loop) ->
             trm_labelled l'
               (trm_seq (* ~annot:(Some Delete_instructions) *) [t_loop; t_del])
          | _ -> fail t_loop.loc "split_loop_nodep_aux: expected labelled loop"
        in
        trm_labelled l (trm_seq [add_del_instr t_loop1; add_del_instr t_loop2])
     | _ -> fail t.loc "split_loop_nodep_aux: bad loop splitting"
     end
  (* otherwise, just split *)
  | Trm_for (init, cond, step, body) ->
     let log : string =
       let loc : string =
         match body.loc with
         | None -> ""
         | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
       in
       Printf.sprintf
         ("  - for (%s; %s; %s) is of the form\n" ^^
          "      for ([int] i = 0; i < N; i++) or " ^^
            "for ([int] i = N; i > 0; i--)\n" ^^
          "  - expression\n%s\n" ^^
          "    %sis of the form\n" ^^
          "      {\n" ^^
          "        {block1}\n" ^^
          "        {block2}\n" ^^
          "      }\n"
         )
         (ast_to_string init) (ast_to_string cond) (ast_to_string step)
         (ast_to_string body) loc
     in
     write_log clog log;
     begin match body.desc with
     | Trm_seq [t_block1; t_block2] ->
        let log : string =
          Printf.sprintf
            ("  - blocks\n%s\n" ^^
             "    and\n%s\n" ^^
             "    are independent\n"
            )
            (ast_to_string t_block1)
            (ast_to_string t_block2)
        in
        write_log clog log;
        trm_labelled result_label
          (trm_seq
             [
               trm_labelled loop1_label (trm_for init cond step t_block1);
               trm_labelled loop2_label (trm_for init cond step t_block2)
             ]
          )
     | _ -> fail t.loc "split_loop_nodep_aux: bad loop body"
     end
  (* the loop might be labelled: delete the label *)
  | Trm_labelled (_, t_loop) ->
     split_loop_nodep_aux clog result_label loop1_label loop2_label t_loop
  | _ -> fail t.loc "split_loop_nodep_aux: not a for loop"

let split_loop_nodep_aux (clog : out_channel) (result_label : string)
  (loop1_label : string) (loop2_label : string) (t : trm) : trm =
  let log : string =
    let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in
    Printf.sprintf
      ("  - %s, %s and %s are fresh labels\n" ^^
       "  - expression\n%s\n" ^^
       "    %sis a (labelled) loop\n"
      )
      result_label loop1_label loop2_label (ast_to_string t) loc
  in
  write_log clog log;
  split_loop_nodep_aux clog result_label loop1_label loop2_label t

let split_loop_nodep (clog : out_channel) (result_label : string)
  (loop1_label : string) (loop2_label : string) (tr : target)
  (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "split_loop_nodep: no matching subterm\n";
     t
  | [dl] ->
     apply_local_transformation
       (split_loop_nodep_aux clog result_label loop1_label loop2_label)
       t
       dl
  | _ ->
     (*
       folding works since no path in epl is the prefix of a subsequent path
      *)
     foldi
       (fun i ->
         let (result_label, loop1_label, loop2_label) =
           let index = string_of_int i in
           (result_label ^ index, loop1_label ^ index, loop2_label ^ index)
         in
         apply_local_transformation
           (split_loop_nodep_aux clog result_label loop1_label loop2_label)
       )
       t
       epl

(*
  -----------DEPRECATED-----------------
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
  | Trm_seq [t_loop; t_del] (*when  t.annot = Some Delete_instructions *) ->
     let t_tiled = tile_loop_aux clog t_loop in
     (* tiled loops are expected to declare their index *)
     begin match t_tiled.desc with
     | Trm_seq [{desc = Trm_for (init1, cond1, step1,
                                 {desc = Trm_seq [body1]; _}); _}; t_del1]
          (* when t_tiled.annot = Some Delete_instructions *) ->
        begin match body1.desc with
        | Trm_seq [{desc = Trm_for (init2, cond2, step2, body); _}; t_del2]
             (* when body1.annot = Some Delete_instructions *) ->
           (* if the index is used in body, then add delete instruction *)
           let i = deleted_var t_del in
           if not (is_used_var_in body i) then t_tiled
           else
             trm_seq (* ~annot:(Some Delete_instructions) *)
               [
                 trm_for init1 cond1 step1
                   (trm_seq
                      [trm_seq (* ~annot:(Some Delete_instructions) *)
                         [
                           trm_for init2 cond2 step2
                             (trm_seq (* ~annot:(Some Delete_instructions) *)
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
          (* when body.annot = Some Delete_instructions *)-> 
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
             trm_seq (* ~annot:(Some Delete_instructions) *)
               (body :: List.rev t_del_l')
          | _ ->
             fail t.loc "tile_loop_aux: bad delete instructions"
        in
        let loop (index : var) (bound : trm) (body : trm) =
          trm_seq (* ~annot:(Some Delete_instructions) *)
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

let tile_loop (clog : out_channel) (tr : target)
  (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
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
