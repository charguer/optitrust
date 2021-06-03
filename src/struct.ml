open Ast
open Target
open Tools

let struct_set_explicit (field_list : var list) : Transfo.t =
  Target.apply_on_target (Struct_core.set_explicit field_list)

let struct_set_implicit : Transfo.t =
  Target.apply_on_target(Struct_core.set_implicit)

let struct_reorder ?(struct_fields : fields = []) ?(move_before : field = "") ?(move_after : field = "") (tg : target) : unit = 
  (* TODO: Ask Arthur about this way of solving the problem *)
  Target.apply_on_target (fun t p ->
    let epl = resolve_target tg t in 
    let field_list = begin match epl with 
    | [dl] -> let (t_def,_) = resolve_path dl t in
      begin match t_def.desc with 
      | Trm_typedef (Typedef_abbrev (_, dx)) ->
        begin match dx.ty_desc with 
        | Typ_struct(l, _, _) -> 
          if struct_fields = [] then l
          else
            begin match move_before, move_after with
            | "",_ -> move_fields_after move_after struct_fields l
            | _, "" -> move_fields_before move_before struct_fields l
            | _,_-> fail t.loc "fields_reorder: only one of move_before or move_after should be specified"
          end
        | _ -> fail t.loc "struct_reorder: the type should be a typedef struct"
        end
      | _ -> fail t.loc "struct_reorder: expected a definition"
      end
    | _ -> fail t.loc "struct_reorder: no target or multiple targets were resolved"
    end
    in
    Struct_core.reorder field_list t p) tg