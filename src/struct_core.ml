open Ast
open Ast_to_c
open Target
open Tools
open Transformations
open Output


(* struct_set_explicit_aux: This is an auxiliary function for struct_set_explicit 
    params: 
      field_list: A string list, each string represents one 
      subt: an ast subterm
    return: 
      the updated ast
 *)
let struct_set_explicit_aux (field_list : var list) (subt : trm) : trm =
  match subt.desc with 
  | Trm_apps(_, [lt;rt]) ->
    begin match rt.desc with
    (* If the right hand side is a get *)
    | Trm_apps(f1, rbase) ->
      begin match lt.desc with
      (* If the variable and the left hand side is heap allocated*)
      | Trm_apps (f2, lbase) ->
        let exp_assgn = List.map (fun sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
         trm_set (trm_apps ~annot:(Some Access) new_f [trm_apps f2 lbase]) (trm_apps ~annot:(Some Access) new_f [trm_apps f1 rbase])
        ) field_list in
       trm_seq ~annot: subt.annot exp_assgn
      (* If the variable at the left hand side is not heap allocated *)
      | Trm_var v ->
        let exp_assgn = List.map(fun sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
        trm_set (trm_apps new_f [trm_var v]) (trm_apps ~annot: (Some Access) f1 [trm_apps new_f rbase])
        ) field_list in
        trm_seq ~annot:subt.annot exp_assgn
      | _ -> fail subt.loc "struct_set_explicit_aux: left term was not matched"
      end
    (* If the right hand side is a struct initialization *)
    | Trm_struct st ->
      begin match lt.desc with 
      | Trm_apps (f2, lbase) ->
        let exp_assgn = List.mapi(fun i sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
        trm_set (trm_apps ~annot:(Some Access) f2 [trm_apps new_f lbase]) (List.nth st i)
        ) field_list in
        trm_seq ~annot:subt.annot exp_assgn
      | Trm_var v ->
        let exp_assgn = List.mapi(fun i sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
        trm_set (trm_apps new_f [trm_var v]) (List.nth st i)
        ) field_list in
        trm_seq ~annot:subt.annot exp_assgn
      | _ -> fail subt.loc "struct_set_explicit_aux: left term was not matched"
      end
    (* Any othe expression *)
    | _ -> 
      let exp_assgn = List.map(fun sf ->
      let new_f = trm_unop (Unop_struct_get sf) in
      trm_set (trm_apps new_f [lt]) (trm_apps new_f [rt])
      ) field_list in
      trm_seq exp_assgn
    end
  | _ -> fail subt.loc "make_explicit_record_assignment_aux: this expression is not supported"
  
(* TODO: Ask Arthur if the user should give the list of fields or we should try to find it automatically *)
(* struct_set_explicit: Transoform a struct set instruction to multiple struct set field instructions   
    params:
      field_list:
        A list of struct field names, used for the explicit assignments
      path_to_set: 
        Path to the set instruction
      t: ast
    return:
      the update ast
 *)
let struct_set_explicit (field_list : var list) (path_to_set : path) (t : trm) : trm =
  apply_local_transformation(struct_set_explicit_aux field_list) t path_to_set

let make_explicit_record_assignment_core (clog : out_channel) (field_list : fields) (trm_index : int) (expression_trm : trm) (t : trm) : trm =
  let log : string =
    let loc : string =
     match t.loc with
     | None -> ""
     | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in Printf.sprintf
    (" -expression\n%s\n" ^^
    "   %sis an assigment\n"
    )
    (ast_to_string expression_trm) loc
    in write_log clog log;
    match t.desc with

    | Trm_seq tl ->
      begin match expression_trm.desc with
      (* TOOD:; check that f is a trm_set *)
      | Trm_apps (f, [lt;rt]) ->
        begin match rt.desc with
        | Trm_apps (f1,rbase) ->
          (* TODO: it might simpler to allows generate rt.x  and then have a cleanup phase
            that is able to compress  access (access foo a) x   into access foo [a;x]
             + with the extra get on the way *)
          begin match lt.desc with (* TODO: define a function is_prim_get_or_access *)
          | Trm_apps ((* ({desc= Trm_val ( Val_prim ( Prim_unop Unop_struct_get _
                             | Prim_unop Unop_struct_access _  )}) as*) f2, lbase) ->

              let exp_assgn = List.map(fun sf ->
              let new_f = trm_unop (Unop_struct_get sf) in
              (* let new_f = {f with desc = Trm_val(Val_prim(Prim_unop (Unop_struct_get sf)))} *)
              trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add ~typ:t.typ
              f [trm_apps ~annot:(Some Access) new_f [trm_apps f2 lbase]; trm_apps ~annot:(Some Access) new_f [trm_apps f1 rbase]]
              ) field_list in
              trm_seq ~annot:t.annot (insert_sublist_in_list exp_assgn trm_index tl)

          | Trm_var v ->
              let exp_assgn = List.map(fun sf ->
              let new_f = trm_unop (Unop_struct_get sf) in
              (* let new_f = {f with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get sf)))} *)
              trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add ~typ:t.typ
              f [trm_apps new_f [trm_var v]; trm_apps ~annot: (Some Access) f1 [trm_apps new_f rbase]]
              ) field_list in
              trm_seq ~annot:t.annot (insert_sublist_in_list exp_assgn trm_index tl)

          | _ -> fail t.loc "make_explicit_record_assignment_aux: left term was not matched"
          end

        | Trm_struct st ->
          begin match lt.desc with
          | Trm_apps (f2,lbase) ->
              let exp_assgn = List.mapi(fun i sf->
                (* let sf = List.nth field_list i in  *)
                let new_f = trm_unop (Unop_struct_get sf) in
                (* let ith_term =  in  *)
                trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add ~typ:t.typ
                    f [trm_apps ~annot:(Some Access) f2 [trm_apps new_f lbase]; List.nth st i]
              ) field_list in
              trm_seq ~annot:t.annot (insert_sublist_in_list exp_assgn trm_index tl)

          | Trm_var v ->
              let exp_assgn = List.mapi(fun i sf ->
                (* let sf = List.nth field_list i in  *)
                let new_f = trm_unop (Unop_struct_get sf) in
                (* let ith_term = List.nth st in  *)
                trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add ~typ:t.typ
                f [trm_apps new_f [trm_var v]; List.nth st i]
              ) field_list in
              trm_seq ~annot:t.annot (insert_sublist_in_list exp_assgn trm_index tl)



          | _ -> fail t.loc "make_explicit_record_assignment_aux: left term was not matched"
          end

        | _ ->

              let exp_assgn = List.map(fun sf ->
              let new_f = trm_unop (Unop_struct_get sf) in
              (* let new_f = {f with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get sf)))} *)
              trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add ~typ:t.typ
              f [trm_apps new_f [lt]; trm_apps new_f [rt]]
              ) field_list in
              trm_seq ~annot:t.annot (insert_sublist_in_list exp_assgn trm_index tl)


        (* fail t.loc "make_explicit_record_assignment_aux: right hand side can only be a value or a variable, function calls are not supported" *)
        end
      | _ -> fail t.loc "make_explicit_record_assignment_aux: this expression is not supported"
      end
    | _ -> fail t.loc "make_explicit_record_assignment_aux: the outer sequence was not matched"

let make_implicit_record_assignment_core (clog : out_channel) (trms_list_size : int) (trm_index : int) (t : trm): trm =
  let rec list_replace_el (el : trm) (i : int) (list : trm list) : 'a list = match list with
    | [] -> failwith "Empty list"
    | x :: xs -> if i = 0 then el :: xs else x :: list_replace_el el (i-1) xs
  in
  let log : string =
    let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in Printf.sprintf
    (" -expression\n%s\n" ^^
    "   %sis a declaration"
    )
    (ast_to_string t) loc

    in write_log clog log;
    match t.desc with
    | Trm_seq tl ->
      let decl = List.nth tl trm_index in
      let assign = List.rev (foldi (fun i acc x -> if i >= trm_index + 1 && i < trm_index + 1 + trms_list_size then x :: acc else acc ) [] tl) in
      let extracted_trms = List.map( fun (sf:trm) ->
        match sf.desc with
        | Trm_apps(_,[_;rt]) -> rt
        | _ -> fail t.loc "make_implicit_record_assignment_aux: all the trms should be assignments"
        ) assign
      in
      
      let var_kind, var_name, var_type = match decl.desc with
        | Trm_let (vk,tx,_) -> vk, fst tx, snd tx
        | _ -> fail t.loc "make_implicit_record_assignment_aux: expected a declaration"
      in
      let new_trm = trm_let var_kind (var_name, var_type) (trm_struct extracted_trms) in
      let tl = list_remove_set assign tl in
      let tl = list_replace_el new_trm trm_index tl in
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "make_implicit_record_assignment_aux: the outer sequence was not matched"


let fields_reorder_core (clog :out_channel) ?(struct_fields : fields = []) ?(move_before : field = "") ?(move_after : field = "")(t : trm) : trm  =
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
    begin match t.desc with
      | Trm_typedef (Typedef_abbrev (x, dx)) ->

        let field_list, field_map =
          match dx.ty_desc with
            | Typ_struct(l,m,_) -> l,m
            |_ -> fail t.loc "fields_reorder: the type should be a typedef struct"
          in
        let reordered_fields =
          match move_before, move_after with
          | "",_ -> move_fields_after move_after struct_fields field_list
          | _, "" -> move_fields_before move_before struct_fields field_list
          | _,_-> fail t.loc "fields_reorder: only one of move_before or move_after should be specified"
          in
        trm_typedef (Typedef_abbrev (x, typ_struct reordered_fields field_map x))


      | _ -> fail t.loc "fields_reorder: expected a definiton"
      end