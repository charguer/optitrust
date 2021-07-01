open Ast
open Target
(* open Tools *)

let set_explicit : Target.Transfo.t =
  Target.apply_on_target (Struct_core.set_explicit)

let set_implicit : Target.Transfo.t =
  Target.apply_on_target(Struct_core.set_implicit)

let reorder ?(move_before : field = "") ?(move_after : field = "") (struct_fields : var list) (tg : target) : unit =
  let move_where,around =
    begin match move_before, move_after with
    | "",_ -> "move_after", move_after
    | _, "" -> "move_before", move_before
    | _,_-> fail None "fields_reorder: only one of move_before or move_after should be specified"
    end
  in
  Target.apply_on_target (Struct_core.reorder struct_fields move_where  around) tg

let inline (field_to_inline : field) : Target.Transfo.t =
  Target.apply_on_transformed_targets(Generic_core.isolate_last_dir_in_seq)
   (fun (p, i) t -> Struct_core.inline field_to_inline i t p)

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

