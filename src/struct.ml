open Ast
open Ast_to_c
open Path
open Path_constructors
open Transformations
open Tools
let make_explicit_record_assignment_aux (clog : out_channel) (field_list : fields) (trm_index : int) (expression_trm : trm) (t : trm) : trm = 
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
              trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr ~add:t.add ~typ:t.typ
              f [trm_apps ~annot:(Some Access) new_f [trm_apps f2 lbase]; trm_apps ~annot:(Some Access) new_f [trm_apps f1 rbase]]
              ) field_list in
              trm_seq ~annot:t.annot (insert_sublist_in_list exp_assgn trm_index tl)

          | Trm_var v ->
              let exp_assgn = List.map(fun sf ->
              let new_f = trm_unop (Unop_struct_get sf) in 
              (* let new_f = {f with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get sf)))} *)
              trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr ~add:t.add ~typ:t.typ
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
                trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr ~add:t.add ~typ:t.typ
                    f [trm_apps ~annot:(Some Access) f2 [trm_apps new_f lbase]; List.nth st i]
              ) field_list in 
              trm_seq ~annot:t.annot (insert_sublist_in_list exp_assgn trm_index tl)

          | Trm_var v ->
              let exp_assgn = List.mapi(fun i sf -> 
                (* let sf = List.nth field_list i in  *)
                let new_f = trm_unop (Unop_struct_get sf) in 
                (* let ith_term = List.nth st in  *)
                trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr ~add:t.add ~typ:t.typ
                f [trm_apps new_f [trm_var v]; List.nth st i]
              ) field_list in 
              trm_seq ~annot:t.annot (insert_sublist_in_list exp_assgn trm_index tl)
              
            
            
          | _ -> fail t.loc "make_explicit_record_assignment_aux: left term was not matched"
          end
        
        | _ ->               

              let exp_assgn = List.map(fun sf ->
              let new_f = trm_unop (Unop_struct_get sf) in 
              (* let new_f = {f with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get sf)))} *)
              trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr ~add:t.add ~typ:t.typ
              f [trm_apps new_f [lt]; trm_apps new_f [rt]]
              ) field_list in 
              trm_seq ~annot:t.annot (insert_sublist_in_list exp_assgn trm_index tl)
 
           
        (* fail t.loc "make_explicit_record_assignment_aux: right hand side can only be a value or a variable, function calls are not supported" *)
        end 
      | _ -> fail t.loc "make_explicit_record_assignment_aux: this expression is not supported"
      end
    | _ -> fail t.loc "make_explicit_record_assignment_aux: the outer sequence was not matched"


let make_explicit_record_assigment (clog : out_channel) ?(struct_name : string = "") (pl : path list) (t : trm) : trm = 
  let struct_def_path = [cType ~name:struct_name ()] in 
  let epl_of_struct_def_path = resolve_path (List.flatten struct_def_path) t in 
  let struct_def_term = match epl_of_struct_def_path with
  | [dl] -> let (t_def,_) = resolve_explicit_path dl t in t_def 
  | _ -> fail t.loc "make_explicit_record_assigment: expected a typedef struct"
  in 
  let field_list = 
  
  match struct_def_term.desc with
  | Trm_decl (Def_typ (_,dx)) -> 
    begin match dx.ty_desc with 
    | Typ_struct (fl,_,_) -> List.rev fl
    | _ -> fail t.loc "make_explicit_record_assigment: the type should be a struct" 
    end
  | _ -> fail t.loc "make_explicit_record_assigment: expected a definition"
  in 
  
  let p = List.flatten pl in 
  let b = !Flags.verbose in 
  Flags.verbose := false;
  let epl = resolve_path p t in 
  Flags.verbose := b;
  let app_transfo   (t : trm) (dl : expl_path) : trm = 
    match List.rev dl with 
    | Dir_nth n :: dl' -> 
      let (t',_) =  resolve_explicit_path dl t in 
      
      let t' = match t'.desc with 
      | Trm_labelled ("detached",t'') -> t''
      | _ -> t'
      in 
      
      let dl = List.rev dl' in 
      apply_local_transformation (make_explicit_record_assignment_aux clog field_list n t') t dl 
    | _ -> fail t.loc "app_transfo: expected a dir_nth inisde the sequence" 
  in 
  (* First check if the path points to a variable declaration *)
  let is_decl = match epl with 
  | [dl] -> let (t_def,_) = resolve_explicit_path dl t in 
    begin match t_def.desc with 
    | Trm_seq[_;_] -> true
    | _ -> false
    end   
  | _ -> fail t.loc "make_explicit_record_assignment: the path should point at one exact term and should not be empty"
  in 
  let t, pl = if is_decl then (detach_expression ~keep_label:true clog pl t,[cLabel ~label:"detached"()])
    else t, pl 
  in 
  
  let new_epl = resolve_path (List.flatten pl) t in 
  
  match new_epl with 
  | [] -> 
    print_info t.loc "make_explicit_record_assignment: no matching subterm";
    t
  | _ -> List.fold_left (fun t dl -> app_transfo t dl) t new_epl
  


let make_implicit_record_assignment_aux (clog : out_channel) (trms_list_size : int) (trm_index : int) (t : trm): trm = 
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
      let var_decl = match decl.desc with 
        | Trm_seq [dc] -> dc 
        | _ -> fail t.loc "make_implicit_record_assignment_aux: expected a declaration"
      in
      let var_name = match var_decl.desc with 
        | Trm_decl(Def_var (x,_)) -> fst x
        | _ -> fail t.loc "make_implicit_record_assignment_aux: expected a declaration"
      in
      let lhs = var_decl in 
      let rhs = trm_set ~annot:(Some Initialisation_instruction) (trm_var var_name) (trm_struct extracted_trms) in
      let new_trm = trm_seq ~annot:(Some Heap_allocated)[lhs;rhs] in 
      let tl = list_remove_set assign tl in 
      let tl = list_replace_el new_trm trm_index tl in  
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "make_implicit_record_assignment_aux: the outer sequence was not matched"
      

 let make_implicit_record_assignment(clog : out_channel) (name : string) (pl : path list) (t : trm) : trm = 
    let struct_term_path = [cType ~name:name ()] in 
    let p_of_struct_term = List.flatten struct_term_path in 
    let epl_of_struct_term = resolve_path p_of_struct_term t in 
    let struct_term = match epl_of_struct_term with 
    | [dl] -> let (t_def,_) = resolve_explicit_path dl t in t_def 
    | _ -> fail t.loc "make_implicit_record_assignment: expected a typedef struct"
    in 
    let fields_list = 
    match struct_term.desc with 
    | Trm_decl (Def_typ (_,dx)) ->
      begin 
      match dx.ty_desc with 
      | Typ_struct (l,_,_) -> l
      | _ -> fail t.loc "make_implicit_record_assignment: the type should be a typedef struct"
      end
    | _ -> fail t.loc "make_implicit_record_assignment: expected a definition"
    in 
    let num_fields = List.length fields_list in 
    let p = List.flatten pl in 
    let b = !Flags.verbose in 
    Flags.verbose := false;
    let epl = resolve_path p t in 
    Flags.verbose := b;
    let app_transfo (t : trm) (dl : expl_path) : trm = 
      match List.rev dl with 
      | Dir_nth n :: dl' ->
        let dl = List.rev dl' in 
        apply_local_transformation (make_implicit_record_assignment_aux clog num_fields n ) t dl 
      | _ -> fail t.loc "app_transfo: expected a dir_nth inisde the sequence"
    in 
    match epl with  
    | [] -> 
      print_info t.loc "make_implicit_record_assignment: no matching subterm";
      t
    | _ -> List.fold_left (fun t dl -> app_transfo t dl) t epl

let fields_reorder_aux (clog :out_channel) ?(struct_fields : fields = []) ?(move_before : field = "") ?(move_after : field = "")(t : trm) : trm  = 
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
      | Trm_decl (Def_typ (x,dx)) ->
        
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
        trm_decl (Def_typ (x, typ_struct reordered_fields field_map x))
        

      | _ -> fail t.loc "fields_reorder: expected a definiton"
      end
    
 

let fields_reorder (clog :out_channel) ?(struct_fields : fields = []) ?(move_before : field = "") ?(move_after : field = "") (pl : path list) (t : trm) : trm  = 
  let p = List.flatten pl in 
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_path p t in 
  Flags.verbose := b;
  match epl with 
  | [] -> 
      print_info t.loc "Struct field reordering\n";
      t
  | _ -> 
      List.fold_left 
        (fun t dl -> 
          apply_local_transformation (fields_reorder_aux clog ~struct_fields ~move_before ~move_after) t dl )
        t
        epl
