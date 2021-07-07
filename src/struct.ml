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
  Target.force_reparse_after
    (Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
      (fun (p, i) t -> Struct_core.inline field_to_inline i t p))

let to_variables : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p, i) t -> Struct_core.to_variables i t p)