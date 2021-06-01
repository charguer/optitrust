open Ast
open Transformations
open Struct_core
open Target

let struct_set_explicit (tg : target) (field_list : var list) : unit =
  apply_to_targets tg (fun p t ->
    Struct_core.struct_set_explicit field_list p t)

let struct_set_implicit (tg : target) : unit =
  apply_to_targets tg (fun p t ->
    Struct_core.struct_set_implicit  p t)

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
          apply_local_transformation (fields_reorder_core clog ~struct_fields ~move_before ~move_after) t dl )
        t
        epl
