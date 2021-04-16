open Ast
open Ast_to_c
open Paths
open Transformations
open Declaration
(*
  instr containing f(arg1, …, argn) is replaced with
  {
    x1 = arg1
    …
    xn = argn
    decl result
    body[x1, …, xn][return r := {result = r; goto return_label}]
    return_label:
      instr[f(arg1, …, argn) := result]
  }
  if tf is void, result won't be used, but instead the empty statement
 *)
let inline_fun_decl ?(inline_at : path list list = [[]]) (result : var)
  (return_label : label) (f : var) (tf : typ) (args : typed_var list)
  (body : trm) (t : trm) : trm =
  (* new names replacing the argument names *)
  let fresh_args = List.map (fun (x, tx) -> (fresh_in t x, tx)) args in
  (* name for the result of f, result might be an argument name *)
  let result =
    fresh_in
      (trm_seq
         (t ::
            List.map
              (fun x_tx -> trm_decl (Def_var (x_tx, trm_lit Lit_uninitialized)))
              fresh_args
         )
      )
      result
  in
  (* result is heap allocated *)
  let result_decl =
    trm_seq ~annot:(Some Heap_allocated)
      [trm_decl (Def_var ((result, typ_ptr tf), trm_prim (Prim_new tf)))]
  in
  (* body where the argument names are substituted *)
  let body =
    List.fold_left
      (fun body (x, _) ->
        change_trm
          (trm_var x)
          (* arguments will be heap allocated *)
          (trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get)
             [trm_var (fresh_in t x)])
          body
      )
      body
      args
  in
  (* body where res is used instead of return statements *)
  let replace_return (t : trm) : trm =
    let rec aux (t : trm) : trm =
      match t.desc with
      (* remove delete instruction related to return statement if any *)
      | Trm_seq tl when t.annot = Some Delete_instructions ->
         begin match List.rev tl with
         | {desc = Trm_abort (Ret (Some r)); _} :: _ ->
            trm_seq ~annot:(Some No_braces) ~loc:t.loc
              [trm_set ~loc:t.loc ~is_instr:true (trm_var result) r;
               trm_goto ~loc:t.loc return_label]
         | {desc = Trm_abort (Ret None); _} :: _ ->
            trm_goto ~loc:t.loc return_label
         | _ -> trm_map aux t
         end
      | Trm_abort (Ret (Some r)) ->
         trm_seq ~annot:(Some No_braces) ~loc:t.loc
           [trm_set ~loc:t.loc ~is_instr:true (trm_var result) r;
            trm_goto ~loc:t.loc return_label]
      | Trm_abort (Ret None) -> trm_goto ~loc:t.loc return_label
      | _ -> trm_map aux t
    in
    clean_up_no_brace_seq (aux t)
  in
  let body = replace_return body in
  let bodyl =
    match body.annot with
    | Some Delete_instructions -> [body]
    | _ ->
       begin match body.desc with
       | Trm_seq tl -> tl
       | _ -> [body]
       end
  in
  (* inline f everywhere in t *)
  let rec apply_change (t : trm) : trm =
    (* we look for instructions that contain a call to f *)
    if not (t.is_instr && contains_call_to_fun f t)
    then trm_map apply_change t
    else
      let arg_vals = fun_call_args f t in
      let arg_decls =
        List.map2
          (fun (x, tx) dx ->
            trm_seq ~annot:(Some Heap_allocated)
              [
                trm_decl (Def_var ((x, typ_ptr tx), trm_prim (Prim_new tx)));
                trm_set ~annot:(Some Initialisation_instruction) (trm_var x) dx
              ]
          )
          fresh_args
          arg_vals
      in
      let arg_dels =
        List.rev_map
          (fun (x, _) ->
            trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ()))
              (trm_unop (Unop_delete false)) [trm_var x]
          )
          fresh_args
      in
      let t =
        match tf.ty_desc with
        | Typ_unit ->
           begin match arg_dels with
           (* if no args, no delete instruction *)
           | [] ->
              trm_seq ~loc:t.loc 
                (bodyl ++
                 [
                   trm_labelled return_label
                     (change_trm (trm_apps (trm_var f) arg_vals)
                        (trm_lit Lit_unit) t)
                 ]
                )
           | _ ->
              trm_seq ~annot:(Some Delete_instructions)
                ((trm_seq ~loc:t.loc
                    (bodyl ++
                     [
                       trm_labelled return_label
                         (change_trm (trm_apps (trm_var f) arg_vals)
                            (trm_lit Lit_unit) t)
                     ]
                    )
                 ) ::
                 arg_dels
                )
           end
        | _ ->
           trm_seq ~annot:(Some Delete_instructions) ~loc:t.loc
             ([
                trm_seq ~loc:t.loc (*REMOVES the braces ~annot:(Some No_braces)*)
                  (arg_decls ++ (result_decl :: bodyl) ++
                   [
                     trm_labelled return_label
                       (change_trm
                          (trm_apps (trm_var f) arg_vals)
                          (trm_apps ~annot:(Some Heap_allocated)
                             (trm_unop Unop_get) [trm_var result])
                          t
                       )
                   ]
                  );
                trm_apps ~annot:(Some Heap_allocated) ~loc:t.loc ~is_instr:true
                  ~typ:(Some (typ_unit ())) (trm_unop (Unop_delete false))
                  [trm_var result]
               ] ++
               arg_dels
             )
      in
      (* clean up *)
      let t = group_decl_init t in
      let t = eliminate_goto_next t in
      let n = nb_goto return_label t in
      if n = 0 then Labels.delete_label return_label t else t
  in
  List.fold_left
    (fun t pl ->
      let p = List.flatten pl in
      let b = !Flags.verbose in
      Flags.verbose := false;
      let epl = resolve_path p t in
      Flags.verbose := b;
      match epl with
      | [] ->
         print_info t.loc "inline_fun_decl: no matching subterm for path %s\n"
           (string_of_path p);
         t
      | _ ->
         List.fold_left (apply_local_transformation apply_change) t epl
    )
    t
    inline_at


(*
  inline the definition pointed at by pl
  paths point at subterms in which all occurences will be replaced
  the empty path means all occurences will be replaced (default behaviour)
  optional argument to remove the declaration (not removed by default)
  assumption for function inlining: the function is used at most once per
  instruction
 *)
let inline_decl (clog : out_channel) ?(delete_decl : bool = false)
  ?(inline_at : path list list = [[]]) ?(fun_result : var = "res")
  ?(fun_return_label : label = "exit") (pl : path list) (t : trm) : trm =
  let p = List.flatten pl in
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_path p t in
  Flags.verbose := b;
  match epl with
  | [dl] ->
     let t =
       let (t_def, _) = resolve_explicit_path dl t in
       let log : string =
         let loc : string =
           match t_def.loc with
           | None -> ""
           | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
         in
         Printf.sprintf
           ("  - expression\n%s\n" ^^
            "    %sis a declaration\n"
           )
           (ast_to_string t_def) loc
       in
       write_log clog log;
       match t_def.desc with
       (* const variables *)
       | Trm_decl (Def_var ((x, _), dx)) ->
          let t_x = trm_var x in
          change_trm ~change_at:inline_at t_x dx t
       (*
         heap allocated variables
         note: an initialisation must be given
        *)
       | Trm_seq [{desc = Trm_decl (Def_var ((x, _), _)); _};
                  {desc = Trm_apps (_, [_; dx]); _}]
            when t_def.annot = Some Heap_allocated ->
          let t_x =
            trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get)
              [trm_var x]
          in
          (*
             make sure x is not replaced in delete instructions by replacing it
             with a fresh variable
           *)
          let x' = fresh_in t x in
          let t =
            change_trm
              (trm_apps (trm_unop (Unop_delete false)) [trm_var x])
              (* do not forget annotations *)
              (trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ()))
                 (trm_unop (Unop_delete false)) [trm_var x'])
              t
          in
          let t = change_trm ~change_at:inline_at t_x dx t in
          (* put back x instead of x' *)
          change_trm (trm_var x') (trm_var x) t
       (* typedef *)
       | Trm_decl (Def_typ (x, dx)) ->
          let ty_x = typ_var x in
          change_typ ~change_at:inline_at ty_x dx t
       (* fun decl *)
       | Trm_decl (Def_fun (f, tf, args, body)) ->
          let log : string =
            Printf.sprintf
              "  - function %s is used at most once per instruction\n"
              f
          in
          write_log clog log;
          inline_fun_decl ~inline_at fun_result fun_return_label f tf args body
            t
       | _ -> fail t.loc "inline_decl: expected a definition"
     in
     if delete_decl then remove_decl clog pl t else t
  | _ -> fail t.loc "inline_decl: the path must point at exactly 1 subterm"

(* let inline_literals (clog : out_channel) (pl : path list) (t : trm) : trm = 
  let rec aux (global : trm) ( t : trm) = 
    match t.desc with 
    | Trm_apps (f,[right_;left]) -> 
      match f.desch with 
      | Trm_val (Val_prim (Prim_binop Binop_add)) ->
        begin match right.desc with 
        | Trm_val (Val_lit ( Lit_int r)) ->
          begin match left.desc with 
          | Trm_val (Val_liet (Lit_int l)) ->
            Trm_val (Val_liet (Lit_int (l+r)))
 *)