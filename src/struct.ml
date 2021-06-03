open Ast
open Target

let set_explicit (field_list : var list) : Target.Transfo.t =
  Target.apply_on_target (Struct_core.set_explicit field_list)

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