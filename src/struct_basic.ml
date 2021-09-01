open Ast
open Target

(* [set_explicit tg] expects [tg] to point to a set instruction where one struct
    instance has been assigned anothe struct instance.
*)
let set_explicit (tg : Target.target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Struct_core.set_explicit i t p) tg

(*  [set_implicit tg] expects [tg] to point to a sequence containing
      a list of struct set assignments. And transorms it into a single
      struct assignment. So it is the inverse of set_explicit.
*)
let set_implicit : Target.Transfo.t =
  Target.apply_on_target(Struct_core.set_implicit)

(* [fields_reorder ?move_before ?move_after struct_fields tg] expects [tg]
    to point to typedef struct. It then switches the order of the fields of
    the targeted struct.
    [move_before] - field before which all the fields in [struct_fields] will be moved
    [move_after] - field after which all the fields in [struct_fields] will be moved
    [struct_fields] - list of fields to move
*)

let fields_reorder ?(move_before : field = "") ?(move_after : field = "") (struct_fields : var list) (tg : target) : unit =
  let move_where =
    Struct_core.(begin match move_before, move_after with
    | "", "" -> Reorder_all
    | "", _ -> Reorder_after move_after
    | _, "" -> Reorder_before move_before
    | _,_-> fail None "fields_reorder: cannot provide both move_before and move_after"
    end) in
  Target.apply_on_target (Struct_core.fields_reorder struct_fields move_where) tg

(* [inline field_to_inline tg] expects [tg] to point to a typedef struct
    it then will find [field_to_inline] and it's underlying type. It will
    replace [field_to_inline] with a list of fields rename comming from the underlying type
    renamed with the following the prefix "field_to_inline_".
*)
let inline (field_to_inline : field) : Target.Transfo.t =
  Target.force_reparse_after
    (Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
      (fun (p, i) t -> Struct_core.inline field_to_inline i t p))

(* [to_variables tg] expects [tg] to point to a variable declaration of type typedef struct.
    Then it will transform this declaration into a list of variable declarations where the type
    of these variables is inherited from the type of the struct definition. All the struct_accesses
    are going to be changed to variable occurrences.
*)
let to_variables : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Struct_core.to_variables i t p)