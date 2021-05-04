open Ast
open Ast_to_c
open Path
open Path_constructors
open Transformations
open Declaration
open Tools
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
let inline_fun_decl ?(inline_at : path list list = [[]]) (result : var)  ?(fun_args : var list = [])
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
      List.fold_left2
        (fun body (x, _) nx ->
          change_trm
            (trm_var x)
            (* TODO: NO LONGER arguments will be heap allocated *)
            (* trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get) 
              [trm_var nx]) *)
            (trm_var nx) 
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
    
    (* we look for instructions that contain a call to f *)
    if not (t.is_instr && contains_call_to_fun f t)
    then trm_map apply_change t
    else
      let arg_vals = fun_call_args f t in
      if List.length fresh_args <> List.length arg_vals 
        then fail t.loc "inline_fun_decl: incorrect number";

      let arg_decls =
        List.map2
          (fun (x, tx) dx ->
            (*trm_seq ~annot:(Some Heap_allocated)
              [
                trm_decl (Def_var ((x, typ_ptr tx), trm_prim (Prim_new tx)));
                trm_set ~annot:(Some Initialisation_instruction) (trm_var x) dx
              ] *)
             trm_decl ~is_instr:true (Def_var ((x, tx), dx))
          )
          fresh_args
          arg_vals
      in
      (*let arg_dels = 
        List.rev_map
          (fun (x, _) ->
            trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ()))
              (trm_unop (Unop_delete false)) [trm_var x]
          )
          fresh_args
      in*)
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
           trm_seq ~annot:(Some Delete_instructions) ~loc:t.loc
             ([
                trm_seq ~loc:t.loc (*REMOVES the braces TODO: braces needed for scopes *) ~annot:(Some No_braces)
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
               ] (*++
               arg_dels*)
             )
      in
      (* clean up *)
      let t = group_decl_init t in
      let t = eliminate_goto_next t in
      let n = nb_goto return_label t in
      if n = 0 then Label.delete_label return_label t else t
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
           (path_to_string p);
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
  ?(inline_at : path list list = [[]]) ?(fun_result : var = "res") ?(fun_args : var list = [])
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
          inline_fun_decl ~inline_at fun_result ~fun_args fun_return_label f tf args body
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

 let inline_seq (clog : out_channel) (pl : path list) (t : trm) : trm =
  let p = List.flatten pl in
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_path p t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "inline_seq: no matching subterm";
     t
  | _ ->
     List.fold_left
       (fun t dl ->
         let log : string =
           let (t, _) = resolve_explicit_path dl t in
           let loc : string =
             match t.loc with
             | None -> ""
             | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
           in
           Printf.sprintf
             ("  - expression\n%s\n" ^^
              "    %sis a seq inside another seq\n"
             )
             (ast_to_string t) loc
         in
         write_log clog log;
         let dl = List.rev dl in
         let n =
           match List.nth_opt dl 0 with
           | Some (Dir_nth n) -> n
           | _ -> fail t.loc "inline_seq: the path must point at a seq element"
         in
         apply_local_transformation
           (fun (t : trm) ->
             match t.desc with
             | Trm_seq tl ->
                let tl =
                  foldi
                    (fun i tl (t : trm) ->
                      if i <> n then tl ++ [t]
                      else
                        match t.desc with
                        | Trm_seq tl' -> tl ++ tl'
                        | _ -> fail t.loc "inline_seq: expected a seq"
                    )
                    []
                    tl
                in
                trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add
                  ~attributes:t.attributes tl
             | _ -> fail t.loc "inline_seq: expected a seq container"
           )
           t
           (* remove the last direction to point at the seq *)
           (List.rev (List.tl dl))
       )
       t
       epl
(* Get the index for a given field of struct inside its list of fields *)
let get_pos (x : typvar) (t : trm) : int = 
  begin match t.desc with 
  | Trm_decl (Def_typ(_,dx)) -> 
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


let inline_record_access_aux (clog : out_channel) (var : string) (field : string) (struct_decl_trm : trm) (list_of_trms : trm list) (t : trm) : trm =  
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
    

let inline_record_access (clog : out_channel) (field : string) (var : string ) (t : trm) : trm = 
      (* Ast_to_text.print_ast ~only_desc:true stdout t; *)
      let pl = [cVarDef ~name:var()] in 
      let epl = resolve_path (List.flatten pl) t in 
      let var_decl = match epl with 
      | [dl] -> let (t_def,_) = resolve_explicit_path dl t in t_def  
            
      | _ ->  fail t.loc "inline_record_access: expected a type"
      in 
      let var_type ,only_decl = match var_decl.desc with 
      | Trm_seq tl->
        let only_decl = if List.length tl = 1 then true else false
        in
        let t_decl = List.hd tl in 
        begin match t_decl.desc with 
        | Trm_decl (Def_var ((x,var_typ),_)) when x = var ->
          begin match var_typ.ty_desc with 
          | Typ_ptr {ty_desc=Typ_var y;_} -> y ,only_decl 
          | _ -> fail t.loc "inline_record_access: type was not matched"
          end 
        | _ -> fail t.loc "inline_record_access: expected a declaration"
        end 
      | _ -> fail t.loc "inline_record_access: could not match the sequnce which contains the declaration"
      in 
      
      let list_of_trms = if not only_decl then match var_decl.desc with 
        
        | Trm_seq [_;t_assign] ->
          begin match t_assign.desc with 
          | Trm_apps (_,[_;lt]) -> 
            begin match lt.desc with
            | Trm_struct tl -> tl 
            | _ -> fail t.loc "implicit_record_assignment: expected a record"
            end
          |  _ -> fail t.loc "implicit_record_assignment: expected an assignment"
          end
        |  _ -> fail t.loc "implicit_record_assignment: expected a sequence term"
        
        else (* search for the trms,
                assumption the variable is only once assigned*) 
          let loc_pl = [cSet ~lhs:[cVar ~name:var ()]()] in 
          let loc_epl = resolve_path (List.flatten loc_pl) t in
          match loc_epl with 
          | [dl] -> let (t_assgn,_) = resolve_explicit_path dl t in 
            begin match t_assgn.desc with 
            | Trm_apps(_,[_;rs]) -> 
              begin match rs.desc with 
              | Trm_struct tl -> tl 
              | _ -> fail t.loc "inline_struct_access: expected a record"
              end 
            | _ -> fail t.loc "inline_struct_access: expected an assignment"
            end 
          | _ -> fail t.loc "inline_struct_access: assumed that the variable was assigned only once"
      in 
      let struct_decl_path = [cType ~name:var_type ()] in 
      let epl_of_struct_decl = resolve_path (List.flatten struct_decl_path) t in 
      let struct_decl_trm  = match epl_of_struct_decl with 
        | [dl] -> let (t_def,_) = resolve_explicit_path dl t in t_def 
        | _ -> fail t.loc "inline_struct_access: expected a typedef struct"
      in 
      let app_transfo (t : trm) (dl : expl_path) : trm = 
        match List.rev dl with 
        | Dir_nth _ :: dl' ->
          let dl = List.rev dl' in 
          apply_local_transformation (inline_record_access_aux clog var field struct_decl_trm list_of_trms) t dl
        | _ -> fail t.loc "inline_struct_access:expected a dir_nth inside the sequence"
      in 
      match epl with 
      | [] ->
        print_info t.loc "inline_struct_access: no matching subterm";
        t
      | _ -> List.fold_left (fun t dl -> app_transfo t dl) t epl 
      (* Ast_to_text.print_ast ~only_desc:true stdout var_decl; *)
     
(* ******************************************************* *)
 (* Auxiliary functions for change_struct_fields function  *)
(* Find all keys which have value = value *)
let find_keys value m = 
  Field_map.fold(fun k v acc -> if v = value then k :: acc else acc) m []

(* A function rename all th elements of a list *)
let rec apply_labels vl pl = match pl with 
| [] -> []
| hd :: tl -> let y = List.map (fun x -> hd ^ "_" ^x) vl in y :: apply_labels vl tl


let add_key key value m = Field_map.add key value m 


let rec add_keys (lk : var list) (lv : typ list) m  = match (lk ,lv) with 
| [],[] -> m
| _ :: _, [] -> m
| [] , _ :: _ -> m
| hd :: tl, hd1 :: tl1 -> let m = add_key hd hd1 m in add_keys tl tl1 m

let rec add_keys_to_map lv llk m = match llk with 
| [] -> m 
| hd :: tl -> let m = add_keys  hd lv m in add_keys_to_map lv tl m
(* ******************************************************* *)


(*
let record_get_typed_fields (fields_list, fields_map) =
                list (string * typ) : list =
                 List.combine fields_list (get_values fields_list fields_map
*)




let change_struct_fields (clog : out_channel) ?(struct_fields : fields = []) (t1 : trm) (t : trm) : trm =
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
    
    let rec add_keys (lv : typ list) (lk : var list) (ov : typ) m  = match (lv ,lk) with 
      | [],[] -> m
      | _ :: _, [] -> m
      | [] , _ :: _ -> m
      | hd :: tl, hd1 :: tl1 -> 

    let m = match ov.ty_desc with
      | Typ_ptr _ ->   add_key hd1 (typ_ptr hd) m
      | Typ_array (_,s) -> add_key hd1 (typ_array hd s ) m 
      | _ -> add_key hd1 hd m 
    in add_keys tl tl1 ov m 
    in 
    let rec add_keys_to_map lv llk olv  m = match (llk,olv) with 
    | [], [] -> m
    | _ :: _, [] -> m
    | [], _ :: _ -> m 
    | hd :: tl ,hd1 :: tl1 -> let m = add_keys lv hd hd1 m in add_keys_to_map lv tl tl1 m 
    in 

    begin match t1.desc with 
      | Trm_decl (Def_typ(_,dx)) -> 
        let field_list, field_map =
          match dx.ty_desc with 
            | Typ_struct (l,m,_) -> l,m
            | _ -> fail t.loc "inline_struct_aux: The type shoudl be a typedef struct"
        in  
        begin match t.desc with 
        | Trm_decl (Def_typ(x1,dx1)) -> 
            let field_list1, field_map1,name = 
              match dx1.ty_desc with
              | Typ_struct(l,m,n) -> l,m,n
              |_ -> fail t.loc "inline_struct_aux: the type should be a typedef struct"
            in 
          
          (* If the list of fields is given then do nothing otherwise find all occurrences of typedef first struct*)
          (* let keys_list = if struct_fields = [] then Field_map.fold(fun k v acc -> if v = x then k :: acc else acc) field_map1 []

            else struct_fields 
            in
          *)
          (* keys_list is the list of struct fields which have to be inlined *)
          let fields_to_inline = struct_fields in 
          (* value_list is the list of values for each field we want to inline, we need that since we have
          to check if there are special types like arrays *)
          let field_types = List.map(fun x -> Field_map.find x field_map1) fields_to_inline in 

          let temp_field_list = apply_labels field_list fields_to_inline in 
          
          (* The key values from the first struct *)
          let values = List.map (fun x -> Field_map.find x field_map) field_list in 

          (* Add the new keys with their values to the second  struct field_map *)
          let field_map1 = add_keys_to_map values temp_field_list field_types field_map1 in 
          

          let field_list1 = insert_list fields_to_inline temp_field_list field_list1 in 
          
          
          let _field_map1 = List.fold_left (fun mapPrev key -> Field_map.remove key mapPrev) field_map1 fields_to_inline in
          
          let field_list1 = list_remove_set  fields_to_inline field_list1 in 

          
          trm_decl (Def_typ (x1,typ_struct field_list1 field_map1 name))
        
        | _ -> fail t.loc "inline_struct_aux: expected a definiton"
        end
      | _ -> fail t.loc " inline_struct_aux: expected a definiton"
      end



let change_struct_access  (x : typvar) (t : trm) : trm = 
  let rec aux (global_trm : trm) (t : trm) : trm = 
    match t.desc with 
    | Trm_apps (f, [base]) ->
      begin match f.desc with 
      | Trm_val (Val_prim (Prim_unop (Unop_struct_access y)))
        | Trm_val (Val_prim (Prim_unop (Unop_struct_get y))) ->
          (* Removed this if else condition just for debugging purposes *)
          (* if false then fail t.loc ("Accessing field " ^ x ^ " is impossible, this field has been deleted during inlining")
          else  *)
          begin match base.desc with 
          | Trm_apps (f',base') ->
            begin match f'.desc with
                        
            | Trm_val(Val_prim (Prim_binop Binop_array_access))
              | Trm_val(Val_prim (Prim_binop Binop_array_get)) ->
                (* THen base caontains another base and also the index  *)
                let base2 = List.nth base' 0 in 
                let index = List.nth base' 1 in 
                begin match base2.desc with 
                | Trm_apps(f'',base3) ->
                  begin match f''.desc with 
                  | Trm_val (Val_prim (Prim_unop Unop_struct_access z))
                    | Trm_val (Val_prim (Prim_unop (Unop_struct_get z ))) when z = x -> 
                    let new_var = z ^ "_" ^ y in 
                    let new_f = {f' with desc = Trm_val(Val_prim (Prim_unop (Unop_struct_access new_var)))} in 
                    trm_apps ~annot:t.annot  f' [trm_apps new_f base3;index]
                  | _ -> trm_map (aux global_trm) t
                  end 
                | _ -> fail t.loc "change_struct_access: expected a trm_apps"
                end 
 
            | Trm_val (Val_prim (Prim_unop (Unop_struct_access z)))
              | Trm_val (Val_prim (Prim_unop (Unop_struct_get z))) when z = x -> 
                let new_var = z ^"_"^ y in
                let new_f = {f' with desc = Trm_val(Val_prim (Prim_unop (Unop_struct_access new_var)))}
              in
              trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr
                     ~add:t.add ~typ:t.typ new_f base'

            | _ -> trm_map (aux global_trm) t
            end
          
          | _ -> trm_map (aux global_trm) t
          end
        
      | _ -> trm_map (aux global_trm) t
      end
    
      (* other cases: recursive call *)
    | _ -> trm_map (aux global_trm) t
in 
aux t t

let change_struct_initialization (_clog : out_channel) (struct_name : typvar) (base_struct_name : typvar) (x : typvar) (t :trm) : trm = 
  let base_struct_path = [cType ~name:base_struct_name()] in 
  let epl_of_base_struct = resolve_path (List.flatten base_struct_path) t in 
  let base_struct_term = match epl_of_base_struct with 
    | [dl] -> let (t_def,_) = resolve_explicit_path dl t in t_def 
    | _ -> fail t.loc "change_struct_initialization: expected a typedef struct"
  in 
  let struct_path = [cType ~name:struct_name ()] in 
  let epl_of_struct = resolve_path (List.flatten struct_path) t in 
  let struct_term = match epl_of_struct with 
  | [dl] -> 
    let (t_def,_) = resolve_explicit_path dl t in t_def 
  | _ -> fail t.loc "change_struct_initialization: expected a typedef struct"
  in 
  
  let pos = get_pos x struct_term in 
  let rec aux (global_trm : trm) (t : trm) = 
    match t.desc with 
    | Trm_struct term_list -> 

      begin match t.typ with 
      | Some{ ty_desc = Typ_var y;_} when y = struct_name -> 
        
        let el = List.nth term_list pos in 
        
        begin match el.desc with 
        | Trm_struct inner_term_list -> trm_struct (insert_sublist_in_list inner_term_list pos term_list)
          
        | Trm_apps(_,[body]) -> 
          
          begin match body.desc with 
          | Trm_var _p ->  (*trm_struct (List.rev term_list)*) 
              let field_list = 
              match base_struct_term.desc with 
                | Trm_decl(Def_typ(_,dx)) ->
                  begin match dx.ty_desc with 
                    | Typ_struct (fl,_,_) -> fl 
                    | _ -> fail t.loc "change_struct_initializaition: expected a struct"
                  end
                | _ -> fail t.loc "change_struct_initialization: expected a definition"
              in 
              let field_list = List.map (fun el -> trm_var (_p ^ "." ^ el)) field_list
              in trm_struct (insert_sublist_in_list field_list pos term_list)
          | _ -> fail t.loc "change_struct_initialization: expected either a record or a variables"
          end
        | _ -> fail t.loc "change_struct_initialization: expected either a record or a variables"
        end
      | _ -> trm_map (aux global_trm) t
      end
    | _ -> trm_map (aux global_trm) t
  in 
  aux t t




(* let change_struct_initialization (_clog : out_channel) (struct_name : typvar) (base_struct_name : typvar) (x : typvar) (t :trm) : trm = 
  let base_struct_path = [cType ~name:base_struct_name()] in 
  let epl_of_base_struct = resolve_path (List.flatten base_struct_path) t in 
  let base_struct_term = match epl_of_base_struct with 
    | [dl] -> let (t_def,_) = resolve_explicit_path dl t in t_def 
    | _ -> fail t.loc "change_struct_initialization: expected a typedef struct"
  in 
  let struct_path = [cType ~name:struct_name ()] in 
  let epl_of_struct = resolve_path (List.flatten struct_path) t in 
  let struct_term = match epl_of_struct with 
  | [dl] -> 
    let (t_def,_) = resolve_explicit_path dl t in t_def 
  | _ -> fail t.loc "change_struct_initialization: expected a typedef struct"
  in 
  
  let pos = get_pos x struct_term in 
  let rec aux (global_trm : trm) (t : trm) = 
    match t.desc with 
    | Trm_struct term_list -> 

      begin match t.typ with 
      | Some{ ty_desc = Typ_var y;_} when y = struct_name -> 
        
        let el = List.nth term_list pos in 
        
        begin match el.desc with 
        | Trm_struct inner_term_list -> trm_struct (insert_sublist_in_list inner_term_list pos term_list)
          
        | Trm_apps(_,[body]) -> 
          
          begin match body.desc with 
          | Trm_var _p ->  (*trm_struct (List.rev term_list)*) 
              let field_list = 
              match base_struct_term.desc with 
                | Trm_decl(Def_typ(_,dx)) ->
                  begin match dx.ty_desc with 
                    | Typ_struct (fl,_,_) -> fl 
                    | _ -> fail t.loc "change_struct_initializaition: expected a struct"
                  end
                | _ -> fail t.loc "change_struct_initialization: expected a definition"
              in 
              let field_list = List.map (fun el -> trm_var (_p ^ "." ^ el)) field_list
              in trm_struct (insert_sublist_in_list field_list pos term_list)
          | _ -> fail t.loc "change_struct_initialization: expected either a record or a variables"
          end
        | _ -> fail t.loc "change_struct_initialization: expected either a record or a variables"
        end
      | _ -> trm_map (aux global_trm) t
      end
    | _ -> trm_map (aux global_trm) t
  in 
  aux t t *)



let inline_struct (clog : out_channel)  ?(struct_fields : fields = []) (name : string) (t : trm) : trm =
  
  let field_name = List.hd struct_fields in 
  
  let struct_term_path  = [cType ~name:name ()] in 
  let p_of_struct_term = List.flatten struct_term_path in
  let epl_of_struct_term = resolve_path p_of_struct_term t in 
  let struct_term = match epl_of_struct_term with 
    | [dl] -> 
      let(t_def,_) = resolve_explicit_path dl t in t_def
    | _ -> fail t.loc "inline_struct: expected a typedef struct"
    in 
  (* Get the type of the field_name by going through the field_map of struct obj *)
  let inner_struct_name = 
  match struct_term.desc with 
  | Trm_decl (Def_typ(_,dx)) ->
    let field_map = 
      match dx.ty_desc with
      | Typ_struct (_,m,_) -> m
      | _ -> fail t.loc "inline_struct: the type should be a typedef struct"
    in 
    let field_map_typ = Field_map.find field_name field_map in 
    begin match field_map_typ.ty_desc with 
    | Typ_var y -> y
    | Typ_array (t_var ,_) -> 
          begin match t_var.ty_desc with 
          | Typ_var y -> y
          | _ -> fail t.loc "inline_struct: expected a typ_var inside the typ_array"
          end 
    
    | Typ_ptr {ty_desc=Typ_var y;_} -> y
    
    | _ -> fail t.loc "inline_struct: expeted a typ var as the value of a key  "
    end 
  | _ -> fail t.loc "inline_struct: expected a definition"
    in 
   
    let  pl_temp = [cType ~name:inner_struct_name()]  in
    let p_temp = List.flatten pl_temp in
    let epl_temp = resolve_path p_temp t in 
    
    (* get the list of fields of the inner struct *)
    let t1 = 
    match epl_temp with
    | [dl] -> 
      let (t_def,_) = resolve_explicit_path dl t in t_def
    | _ -> fail t.loc "inline_struct: expected a typedef struct"
    in
    
    
   let t =  List.fold_left (fun acc_t x -> change_struct_access x acc_t) t struct_fields
    in
    
     let t = List.fold_left (fun acc_t x -> change_struct_initialization  clog  name inner_struct_name x acc_t ) t struct_fields
    in 
    
    let p = List.flatten struct_term_path in
    let b = !Flags.verbose in
    Flags.verbose := false;
    let epl = resolve_path p t in
    Flags.verbose := b;
    match epl with 
    | [] -> 
      print_info t.loc "inline_struct: no matching subterm";
      t
    | _ -> 
      List.fold_left 
        (fun t dl -> 
          apply_local_transformation (change_struct_fields clog ~struct_fields t1) t dl)
        t
        epl 