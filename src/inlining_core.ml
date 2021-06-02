open Ast
open Ast_to_c
open Clang_to_ast
open Target
open Generic
open Declaration
open Tools
open Output

(* TODO: Remove current inline_fun_decl from inline_decl, these two functions should be independent *)
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
let inline_fun_decl ?(inline_at : target list = [[]]) (result : var)  ?(fun_args : var list = [])
  (return_label : label) (f : var) (tf : typ) (args : typed_var list)
  (body : trm) (t : trm) : trm =

  (*let counter = ref 0 in  ^ string_of_int !counter *)
  (* inline f everywhere in t *)
  let rec apply_change (t : trm) : trm =
    (*Ast_to_text.print_ast ~only_desc:true stdout t; *) (* TODO: make show_ast  accessible from everywhere*)
    (* new names replacing the argument names *)
    (* DEPRECATED incr counter; *)
  let fresh_args =
     match fun_args with
     | [] -> List.map (fun (x, tx) -> (fresh_in t x, tx)) args
     | _ ->
      if List.length fun_args <> List.length args
        then fail t.loc "inline_fun_decl: incorrect number of names provided for the arguments";
       List.map2 (fun (_x, tx) nx -> (nx, tx)) args fun_args
     in

    (* name for the result of f, result might be an argument name *)
  let result =
      fresh_in
        (trm_seq
          (t ::
              List.map
                (fun x_tx -> trm_let Var_mutable x_tx (trm_lit Lit_uninitialized))
                fresh_args
          )
        )
        result
    in
    (* result is heap allocated *)
    let result_decl =
      trm_let Var_mutable (result, typ_ptr tf) (trm_prim (Prim_new tf))
      
    in
    (* body where the argument names are substituted *)
    let body =
      List.fold_left2
        (fun body (x, _) nx ->
          change_trm
            (trm_var x)
            (trm_apps ~annot:(Some Mutable_var_get) (trm_unop Unop_get)
              [trm_var nx])
            body
        )
        body
        args fun_args
    in
    (* body where res is used instead of return statements *)
    let replace_return (t : trm) : trm =
      let rec aux (t : trm) : trm =
        match t.desc with
        (* remove delete instruction related to return statement if any *)
        (* | Trm_seq tl when t.annot = Some Delete_instructions ->
          begin match List.rev tl with
          | {desc = Trm_abort (Ret (Some r)); _} :: _ ->
              trm_seq ~annot:(Some No_braces) ~loc:t.loc
                [trm_set ~loc:t.loc ~is_statement:true (trm_var result) r;
                trm_goto ~loc:t.loc return_label]
          | {desc = Trm_abort (Ret None); _} :: _ ->
              trm_goto ~loc:t.loc return_label
          | _ -> trm_map aux t
          end *)
        | Trm_abort (Ret (Some r)) ->
          trm_seq ~annot:(Some No_braces) ~loc:t.loc
            [trm_set ~loc:t.loc ~is_statement:true (trm_var result) r;
              trm_goto ~loc:t.loc return_label]
        | Trm_abort (Ret None) -> trm_goto ~loc:t.loc return_label
        | _ -> trm_map aux t
      in
      clean_up_no_brace_seq (aux t)
    in
    let body = replace_return body in
    let bodyl =
      match body.annot with
      (* | Some Delete_instructions -> [body] *)
      | _ ->
        begin match body.desc with
        | Trm_seq tl -> tl
        | _ -> [body]
        end
    in

    (* we look for instructions that contain a call to f *)
    if not (t.is_statement && contains_call_to_fun f t)
    then trm_map apply_change t
    else
      let arg_vals = fun_call_args f t in
      if List.length fresh_args <> List.length arg_vals
        then fail t.loc "inline_fun_decl: incorrect number";

      let arg_decls =
        List.map2
          (fun (x, tx) dx ->
             trm_let ~is_statement:true Var_mutable (x,tx) dx
          )
          fresh_args
          arg_vals
      in
     
      let t =
        match tf.ty_desc with
        | Typ_unit ->
            trm_seq ~loc:t.loc ~annot:(Some No_braces)
                (arg_decls ++ bodyl ++
                 [
                   trm_labelled return_label
                     (change_trm (trm_apps (trm_var f) arg_vals)
                        (trm_lit Lit_unit) t)
                 ]
                )
                (*
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
           end*)
        | _ ->
           trm_seq(*  ~annot:(Some Delete_instructions) *) ~loc:t.loc
             ([
                trm_seq ~loc:t.loc (*REMOVES the braces TODO: braces needed for scopes *) ~annot:(Some No_braces)
                  (arg_decls ++ (result_decl :: bodyl) ++
                   [
                     trm_labelled return_label
                       (change_trm
                          (trm_apps (trm_var f) arg_vals)
                          (* TODO: Fix this later *)
                          (trm_apps ~annot:(Some Mutable_var_get)
                             (trm_unop Unop_get) [trm_var result])
                          t
                       )
                   ]
                  );
               ] 
             )
      in
      (* clean up *)
      let t = group_decl_init t in
      let t = eliminate_goto_next t in
      let n = nb_goto return_label t in
      if n = 0 then Label.delete_label return_label t else t
  in
  List.fold_left
    (fun t tr ->
      let b = !Flags.verbose in
      Flags.verbose := false;
      let epl = resolve_target tr t in
      Flags.verbose := b;
      match epl with
      | [] ->
         print_info t.loc "inline_fun_decl: no matching subterm for path %s\n"
           (target_to_string tr);
         t
      | _ ->
         List.fold_left (apply_local_transformation apply_change) t epl
    )
    t
    inline_at

let inline_decl_core (clog : out_channel) (inline_at : target list) (fun_result : var)
    (fun_args : var list) (fun_return_label : label)(t : trm) : trm =
    let log : string =
         let loc : string =
           match t.loc with
           | None -> ""
           | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
         in
         Printf.sprintf
           ("  - expression\n%s\n" ^^
            "    %sis a declaration\n"
           )
           (ast_to_string t) loc
       in
       write_log clog log;
       match t.desc with
       (* const variables *)
       | Trm_let (_,(x,_), dx) ->
          let t_x = trm_var x in
          change_trm ~change_at:inline_at t_x dx t
       (*
         heap allocated variables
         note: an initialisation must be given
        *)
       (* | Trm_seq [{desc = Trm_decl (Def_var ((x, _), _)); _};
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
            (* change_trm
              (trm_apps (trm_unop (Unop_delete false)) [trm_var x])
              (* do not forget annotations *)
              (trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ()))
                 (trm_unop (Unop_delete false)) [trm_var x']) *)
              t
          in
          let t = change_trm ~change_at:inline_at t_x dx t in
          (* put back x instead of x' *)
          change_trm (trm_var x') (trm_var x) t *)
       (* typedef *)
       | Trm_typedef (Typedef_abbrev (x,dx)) ->
          let ty_x = typ_var x (get_typedef x) in
          change_typ ~change_at:inline_at ty_x dx t
       (* fun decl *)
       | Trm_let_fun (f, tf, args, body) ->
          let log : string =
            Printf.sprintf
              "  - function %s is used at most once per instruction\n"
              f
          in
          write_log clog log;
          inline_fun_decl ~inline_at fun_result ~fun_args fun_return_label f tf args body
            t
       | _ -> fail t.loc "inline_decl: expected a definition"
     

(* Get the index for a given field of struct inside its list of fields *)
let get_pos (x : typvar) (t : trm) : int =
  begin match t.desc with
    | Trm_typedef (Typedef_abbrev(_, dx)) ->
       let field_list1 =
          match dx.ty_desc with
          | Typ_struct(l,_,_) -> l
          |_ -> fail t.loc "get_pos: the type should be a typedef struct"
        in

        let rec find x lst =
        match lst with
        | [] -> raise (Failure "Not Found")
        | hd :: tl -> if hd = x then 0 else 1 + find x tl
        in
        find x field_list1
    | _ -> fail t.loc "get_pos_and_element: expected a struct type"
    end


let inline_record_access_core (clog : out_channel) (var : string) (field : string) (struct_decl_trm : trm) (list_of_trms : trm list) (t : trm) : trm =
  let log : string =
  let loc : string =
    match t.loc with
    | None -> ""
    | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
  in Printf.sprintf
    (" -expresssion\n%s\n" ^^
    "  %sis an assignment with record access\n"
    )
    (ast_to_string t) loc
    in write_log clog log;
    (* search for the declaration of the variable *)
  let rec aux (global_trm : trm ) (t : trm) : trm =
      begin match t.desc with
      | Trm_apps (f,[base]) ->
        begin match f.desc with
        | Trm_val (Val_prim (Prim_unop (Unop_struct_access y)))
          | Trm_val (Val_prim (Prim_unop (Unop_struct_get y))) when y = field ->
          begin match base.desc with
          | Trm_var v when v = var ->
            let index = get_pos field struct_decl_trm in
            List.nth (List.rev list_of_trms) index
          | _ -> trm_map (aux global_trm) t
          end
        | _ -> trm_map (aux global_trm) t
        end
      | _ -> trm_map (aux global_trm) t
      end
    in aux t t