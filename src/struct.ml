open Ast
open Ast_to_c
open Target
open Path_constructors
open Transformations
open Tools

let make_explicit_record_assigment (clog : out_channel) ?(struct_name : string = "") (tr : target) (t : trm) : trm =
  let struct_def_path = [cTypDef struct_name] in
  let epl_of_struct_def_path = resolve_target struct_def_path t in
  let struct_def_term = match epl_of_struct_def_path with
  | [dl] -> let (t_def,_) = resolve_path dl t in t_def
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

  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  let app_transfo   (t : trm) (dl : path) : trm =
    match List.rev dl with
    | Dir_nth n :: dl' ->
      let (t',_) =  resolve_path dl t in

      let t' = match t'.desc with
      | Trm_labelled ("detached",t'') -> t''
      | _ -> t'
      in

      let dl = List.rev dl' in
      apply_local_transformation (make_explicit_record_assignment_core clog field_list n t') t dl
    | _ -> fail t.loc "app_transfo: expected a dir_nth inisde the sequence"
  in
  (* First check if the path points to a variable declaration *)
  let is_decl = match epl with
  | [dl] -> let (t_def,_) = resolve_path dl t in
    begin match t_def.desc with
    | Trm_seq[_;_] -> true
    | _ -> false
    end
  | _ -> fail t.loc "make_explicit_record_assignment: the path should point at one exact term and should not be empty"
  in
  let t, tr = if is_decl then (detach_expression ~keep_label:true clog tr t,[cLabel "detached"])
    else t, tr
  in

  let new_epl = resolve_target tr t in

  match new_epl with
  | [] ->
    print_info t.loc "make_explicit_record_assignment: no matching subterm";
    t
  | _ -> List.fold_left (fun t dl -> app_transfo t dl) t new_epl

 let make_implicit_record_assignment(clog : out_channel) (name : string) (tr : target) (t : trm) : trm =
    let struct_term_path = [cTypDef name] in
    let p_of_struct_term = struct_term_path in
    let epl_of_struct_term = resolve_target p_of_struct_term t in
    let struct_term = match epl_of_struct_term with
    | [dl] -> let (t_def,_) = resolve_path dl t in t_def
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
    let b = !Flags.verbose in
    Flags.verbose := false;
    let epl = resolve_target tr t in
    Flags.verbose := b;
    let app_transfo (t : trm) (dl : path) : trm =
      match List.rev dl with
      | Dir_nth n :: dl' ->
        let dl = List.rev dl' in
        apply_local_transformation (make_implicit_record_assignment_core clog num_fields n ) t dl
      | _ -> fail t.loc "app_transfo: expected a dir_nth inisde the sequence"
    in
    match epl with
    | [] ->
      print_info t.loc "make_implicit_record_assignment: no matching subterm";
      t
    | _ -> List.fold_left (fun t dl -> app_transfo t dl) t epl

let fields_reorder (clog :out_channel) ?(struct_fields : fields = []) ?(move_before : field = "") ?(move_after : field = "") (tr : target) (t : trm) : trm  =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
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
