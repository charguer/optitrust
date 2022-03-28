open Ast
open Target
include Struct_core
include Struct_core.Rename

(* [set_explicit tg] expects [tg] to point to a set instruction where one struct
    instance has been assigned anothe struct instance.
*)
let set_explicit (tg : Target.target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
  Target.apply_on_targets(Struct_core.set_explicit) tg)

(*  [set_implicit tg] expects [tg] to point to a sequence containing
      a list of struct set assignments. And transorms it into a single
      struct assignment. So it is the inverse of set_explicit.
*)
let set_implicit ?(keep_label : bool = true) : Target.Transfo.t =
  Target.apply_on_targets (Struct_core.set_implicit keep_label)

(* [fields_reorder ?move_before ?move_after struct_fields tg] expects [tg]
    to point to typedef struct. It then switches the order of the fields of
    the targeted struct.
    [move_before] - field before which all the fields in [struct_fields] will be moved
    [move_after] - field after which all the fields in [struct_fields] will be moved
    [struct_fields] - list of fields to move

   @correctness: Correct if pointer arithmetic to field is replaced everywhere,
   might be impossible to prove in case of casts between types.
*)

let fields_reorder ?(move_before : field = "") ?(move_after : field = "") (struct_fields : vars) (tg : target) : unit =
  let move_where =
    begin match move_before, move_after with
    | "", "" -> Reorder_all
    | "", _ -> Reorder_after move_after
    | _, "" -> Reorder_before move_before
    | _,_-> fail None "fields_reorder: cannot provide both move_before and move_after"
    end in
  Target.apply_on_targets (Struct_core.fields_reorder struct_fields move_where) tg

(* [reveal field_to_reveal tg] expects [tg] to point to a typedef struct
    it then will find [field_to_reveal] and it's underlying type. It will
    replace [field_to_reveal] with a list of fields rename comming from the underlying type
    renamed with the following the prefix "field_to_reveal_".
*)
let reveal ?(reparse:bool=false) (field_to_reveal : field) : Target.Transfo.t =
  Target.reparse_after ~reparse
    (Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
      (fun t (p, i) -> Struct_core.reveal field_to_reveal i t p))

(* [to_variables tg] expects [tg] to point to a variable declaration of type typedef struct.
    Then it will transform this declaration into a list of variable declarations where the type
    of these variables is inherited from the type of the struct definition. All the struct_accesses
    are going to be changed to variable occurrences.
*)
let to_variables : Target.Transfo.t =
  Target.reparse_after ~reparse:false (Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p, i) -> Struct_core.to_variables i t p) )

(* [rename_fields rename tg] expects [tg] to point to a struct declaration 
    then it will rename all the fields which are matched when applying the type rename
    which can be a function to renam all the struct fields or only those which 
    are matched by the patter given as argument when the function [only_for] is used.
*)
let rename_fields (rename : rename) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p, i) -> Struct_core.rename_fields i rename t p)

(* [applyto_fields_type ~reparse pattern typ_update tg] expects the target [ŧg] to be pointing at
    struct definition, then it will update all the struct fields type whose identifier matches [pattern]*)
let applyto_fields_type ?(reparse : bool = false) (pattern : string) (typ_update: typ -> typ) : Target.Transfo.t =
  Target.reparse_after ~reparse (Target.apply_on_targets (Struct_core.update_fields_type pattern typ_update))

(* [update_fields_type pattern ty tg] expects [tg] to point to a struct declaration . 
    Then it will change the current type to [ty] of all the fields which are matched with [pattern].
*)
let update_fields_type ?(reparse : bool = false) (pattern : string) (ty : typ) : Target.Transfo.t =
  applyto_fields_type ~reparse pattern (fun _ -> ty)

(* [simpl_proj tg] expects the target [tg] pointing to any node whose descendants can contain struct
 initialization list projections 
*)
let simpl_proj : Target.Transfo.t =
  Target.apply_on_targets (Struct_core.simpl_proj)
