open Ast
open Target
(* open Tools *)

let set_explicit : Target.Transfo.t =
  Target.apply_on_target (Struct_core.set_explicit)

let set_implicit : Target.Transfo.t =
  Target.apply_on_target(Struct_core.set_implicit)

let reorder ?(move_before : field = "") ?(move_after : field = "") (struct_fields : var list) (tg : target) : unit =
  (* TODO: Ask Arthur about this way of solving the problem *)
  let move_where,around =
    begin match move_before, move_after with
    | "",_ -> "move_after", move_after
    | _, "" -> "move_before", move_before
    | _,_-> fail None "fields_reorder: only one of move_before or move_after should be specified"
    end
  in
  Target.apply_on_target (Struct_core.reorder struct_fields move_where  around) tg



let inline_record_access (field : string) (var : string ) (t : trm) : trm =
      (* Ast_to_text.print_ast ~only_desc:true stdout t; *)
      let pl = [cVarDef var] in
      let epl = resolve_target pl t in
      let var_decl = match epl with
      | [dl] -> let (t_def,_) = Path.resolve_path dl t in t_def

      | _ ->  fail t.loc "inline_record_access: expected a type"
      in
      let var_type ,only_decl = match var_decl.desc with
      | Trm_seq tl->
        let only_decl = if List.length tl = 1 then true else false
        in
        let t_decl = List.hd tl in
        begin match t_decl.desc with
        | Trm_let (_,(x, var_typ), _) when x = var ->
          begin match var_typ.typ_desc with
          | Typ_ptr {typ_desc=Typ_constr (y, _, _);_} -> y ,only_decl
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
          (* let loc_pl = [cSet ~lhs:[cVar var ]()] in  *)
          let loc_pl = cSet ~lhs:[cVar var] ()  in
          let loc_epl = resolve_target loc_pl t in
          match loc_epl with
          | [dl] -> let (t_assgn,_) = Path.resolve_path dl t in
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
      let struct_decl_path = [cTypDef var_type ] in
      let epl_of_struct_decl = resolve_target struct_decl_path t in
      let struct_decl_trm  = match epl_of_struct_decl with
        | [dl] -> let (t_def,_) = Path.resolve_path dl t in t_def
        | _ -> fail t.loc "inline_struct_access: expected a typedef struct"
      in
      let app_transfo (t : trm) (dl : path) : trm =
        match List.rev dl with
        | Dir_nth _ :: dl' ->
          let dl = List.rev dl' in
          apply_on_path (Struct_core.inline_record_access_core  var field struct_decl_trm list_of_trms) t dl
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
  String_map.fold(fun k v acc -> if v = value then k :: acc else acc) m []

(* A function rename all th elements of a list *)
let rec apply_labels vl pl = match pl with
| [] -> []
| hd :: tl -> let y = List.map (fun x -> hd ^ "_" ^x) vl in y :: apply_labels vl tl


let add_key key value m = String_map.add key value m


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

(* let change_struct_fields ?(struct_fields : fields = []) (t1 : trm) (t : trm) : trm =
      let rec add_keys (lv : typ list) (lk : var list) (ov : typ) m  = match (lv ,lk) with
      | [],[] -> m
      | _ :: _, [] -> m
      | [] , _ :: _ -> m
      | hd :: tl, hd1 :: tl1 ->

    let m = match ov.typ_desc with
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
      | Trm_typedef (Typedef_abbrev (_, dx)) ->
        let field_list, field_map =
          match dx.typ_desc with
            | Typ_struct (l,m,_) -> l,m
            | _ -> fail t.loc "inline_struct_aux: The type shoudl be a typedef struct"
        in
        begin match t.desc with
        | Trm_typedef (Typedef_abbrev (x1, dx1)) ->
            let field_list1, field_map1,name =
              match dx1.typ_desc with
              | Typ_struct(l,m,n) -> l,m,n
              |_ -> fail t.loc "inline_struct_aux: the type should be a typedef struct"
            in

          (* If the list of fields is given then do nothing otherwise find all occurrences of typedef first struct*)
          (* let keys_list = if struct_fields = [] then String_map.fold(fun k v acc -> if v = x then k :: acc else acc) field_map1 []

            else struct_fields
            in
          *)
          (* keys_list is the list of struct fields which have to be inlined *)
          let fields_to_inline = struct_fields in
          (* value_list is the list of values for each field we want to inline, we need that since we have
          to check if there are special types like arrays *)
          let field_types = List.map(fun x -> String_map.find x field_map1) fields_to_inline in

          let temp_field_list = apply_labels field_list fields_to_inline in

          (* The key values from the first struct *)
          let values = List.map (fun x -> String_map.find x field_map) field_list in

          (* Add the new keys with their values to the second  struct field_map *)
          let field_map1 = add_keys_to_map values temp_field_list field_types field_map1 in


          let field_list1 = insert_list fields_to_inline temp_field_list field_list1 in


          let _field_map1 = List.fold_left (fun mapPrev key -> String_map.remove key mapPrev) field_map1 fields_to_inline in

          let field_list1 = list_remove_set  fields_to_inline field_list1 in

          trm_typedef (Typedef_abbrev (x1, typ_struct field_list1 field_map1 name))

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
              trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
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
  let base_struct_path = [cTypDef base_struct_name] in
  let epl_of_base_struct = resolve_target base_struct_path t in
  let base_struct_term = match epl_of_base_struct with
    | [dl] -> let (t_def,_) = Path.resolve_path dl t in t_def
    | _ -> fail t.loc "change_struct_initialization: expected a typedef struct"
  in
  let struct_path = [cTypDef struct_name ] in
  let epl_of_struct = resolve_target struct_path t in
  let struct_term = match epl_of_struct with
  | [dl] ->
    let (t_def,_) = Path.resolve_path dl t in t_def
  | _ -> fail t.loc "change_struct_initialization: expected a typedef struct"
  in

  let pos = Struct_core.get_pos x struct_term in
  let rec aux (global_trm : trm) (t : trm) =
    match t.desc with
    | Trm_struct term_list ->

      begin match t.typ with
      | Some{ typ_desc = Typ_constr (y, _, _); _} when y = struct_name ->

        let el = List.nth term_list pos in

        begin match el.desc with
        | Trm_struct inner_term_list -> trm_struct (insert_sublist_in_list inner_term_list pos term_list)

        | Trm_apps(_,[body]) ->

          begin match body.desc with
          | Trm_var _p ->  (*trm_struct (List.rev term_list)*)
              let field_list =
              match base_struct_term.desc with
                | Trm_typedef (Typedef_abbrev (_, dx)) ->
                  begin match dx.typ_desc with
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




(* let change_struct_initialization (_clog : out_channel) (struct_name : typvar) (base_struct_name : typvar) (x : typvar) (t :trm) : trm =
  let base_struct_path = [cTypDef base_struct_name] in
  let epl_of_base_struct = resolve_target (
     base_struct_path) t in
  let base_struct_term = match epl_of_base_struct with
    | [dl] -> let (t_def,_) = resolve_target dl t in t_def
    | _ -> fail t.loc "change_struct_initialization: expected a typedef struct"
  in
  let struct_path = [cTypDef struct_name] in
  let epl_of_struct = resolve_target (List.flatten struct_path) t in
  let struct_term = match epl_of_struct with
  | [dl] ->
    let (t_def,_) = resolve_target dl t in t_def
  | _ -> fail t.loc "change_struct_initialization: expected a typedef struct"
  in

  let pos = get_pos x struct_term in
  let rec aux (global_trm : trm) (t : trm) =
    match t.desc with
    | Trm_struct term_list ->

      begin match t.typ with
      | Some{ typ_desc = Typ_constr (y, _, _); _} when y = struct_name ->

        let el = List.nth term_list pos in

        begin match el.desc with
        | Trm_struct inner_term_list -> trm_struct (insert_sublist_in_list inner_term_list pos term_list)

        | Trm_apps(_,[body]) ->

          begin match body.desc with
          | Trm_var _p ->  (*trm_struct (List.rev term_list)*)
              let field_list =
              match base_struct_term.desc with
                | Trm_typedef (Typedef_abbrev (_, dx)) ->
                  begin match dx.typ_desc with
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

(* TODO: Re-implement from scratch this function *)
(* let inline_struct (clog : out_channel)  ?(struct_fields : fields = []) (name : string) (t : trm) : trm =

  let field_name = List.hd struct_fields in

  let struct_term_path  = [cTypDef name] in
  let p_of_struct_term = struct_term_path in
  let epl_of_struct_term = resolve_target p_of_struct_term t in
  let struct_term = match epl_of_struct_term with
    | [dl] ->
      let(t_def,_) = Path.resolve_path dl t in t_def
    | _ -> fail t.loc "inline_struct: expected a typedef struct"
    in
  (* Get the type of the field_name by going through the field_map of struct obj *)
  let inner_struct_name =
  match struct_term.desc with
  | Trm_typedef (Typedef_abbrev (_, dx)) ->
    let field_map =
      match dx.typ_desc with
      | Typ_struct (_,m,_) -> m
      | _ -> fail t.loc "inline_struct: the type should be a typedef struct"
    in
    let field_map_typ = String_map.find field_name field_map in
    begin match field_map_typ.typ_desc with
    | Typ_constr (y, _, _) -> y
    | Typ_array (t_var ,_) ->
          begin match t_var.typ_desc with
          | Typ_constr (y, _, _) -> y
          | _ -> fail t.loc "inline_struct: expected a typ_var inside the typ_array"
          end

    | Typ_ptr {typ_desc=Typ_constr (y, _, _); _} -> y

    | _ -> fail t.loc "inline_struct: expeted a typ var as the value of a key  "
    end
  | _ -> fail t.loc "inline_struct: expected a definition"
    in

    let  pl_temp = [cTypDef inner_struct_name]  in
    let p_temp = pl_temp in
    let epl_temp = resolve_target p_temp t in

    (* get the list of fields of the inner struct *)
    let t1 =
    match epl_temp with
    | [dl] ->
      let (t_def,_) = Path.resolve_path dl t in t_def
    | _ -> fail t.loc "inline_struct: expected a typedef struct"
    in


   let t =  List.fold_left (fun acc_t x -> change_struct_access x acc_t) t struct_fields
    in

     let t = List.fold_left (fun acc_t x -> change_struct_initialization  clog  name inner_struct_name x acc_t ) t struct_fields
    in

    let tr = struct_term_path in
    let b = !Flags.verbose in
    Flags.verbose := false;
    let epl = resolve_target tr t in
    Flags.verbose := b;
    match epl with
    | [] ->
      print_info t.loc "inline_struct: no matching subterm";
      t
    | _ ->
      List.fold_left
        (fun t dl ->
          apply_on_path (change_struct_fields ~struct_fields t1) t dl)
        t
        epl *)