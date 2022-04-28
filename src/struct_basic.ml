open Ast
open Target
include Struct_core
include Struct_core.Rename

(* [set_explicit tg] expects [tg] to point at a set instruction where one struct
    instance has been assigned anothe struct instance.
*)
let set_explicit (tg : target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
  apply_on_targets(Struct_core.set_explicit) tg)

(*  [set_implicit tg] expects [tg] to point at a sequence containing
      a list of struct set assignments. And transorms it into a single
      struct assignment. So it is the inverse of set_explicit.
*)
let set_implicit ?(keep_label : bool = true) : Transfo.t =
  apply_on_targets (Struct_core.set_implicit keep_label)

(* [fields_reorder ?move_before ?move_after struct_fields tg] expects [tg]
    to point attypedef struct. It then switches the order of the fields of
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
  apply_on_targets (Struct_core.fields_reorder struct_fields move_where) tg

(* [reveal_field field_to_reveal_field tg] expects [tg] to point at a typedef struct
    it then will find [field_to_reveal_field] and it's underlying type. It will
    replace [field_to_reveal_field] with a list of fields rename comming from the underlying type
    renamed with the following the prefix "field_to_reveal_field_".
*)
let reveal_field ?(reparse:bool=false) (field_to_reveal_field : field) : Transfo.t =
  reparse_after ~reparse
    (apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
      (fun t (p, i) -> Struct_core.reveal_field field_to_reveal_field i t p))

(* [reveal_fields fields_to_reveal_field tg] an extension to the reveal_field transformation but this one 
     is applied on multiple struct fields *)
let reveal_fields ?(reparse : bool = false) (fields_to_reveal_field : fields) (tg : target) : unit =
  List.iter (fun f ->  reveal_field f tg) fields_to_reveal_field


(* [to_variables tg] expects [tg] to point at a variable declaration of type typedef struct.
    Then it will transform this declaration into a list of variable declarations where the type
    of these variables is inherited from the type of the struct definition. All the struct_accesses
    are going to be changed to variable occurrences.
*)
let to_variables : Transfo.t =
  reparse_after ~reparse:false (apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p, i) -> Struct_core.to_variables i t p) )

(* [rename_fields rename tg] expects [tg] to point at a struct declaration 
    then it will rename all the fields which are matched when applying the type rename
    which can be a function to renam all the struct fields or only those which 
    are matched by the patter given as argument when the function [only_for] is used.
*)
let rename_fields (rename : rename) : Transfo.t =
  apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p, i) -> Struct_core.rename_fields i rename t p)

(* [applyto_fields_type ~reparse pattern typ_update tg] expects the target [ŧg] to be pointing at
    struct definition, then it will update all the struct fields type whose identifier matches [pattern]*)
let applyto_fields_type ?(reparse : bool = false) (pattern : string) (typ_update: typ -> typ) : Transfo.t =
  reparse_after ~reparse (apply_on_targets (Struct_core.update_fields_type pattern typ_update))

(* [update_fields_type pattern ty tg] expects [tg] to point at a struct declaration . 
    Then it will change the current type to [ty] of all the fields which are matched with [pattern].
*)
let update_fields_type ?(reparse : bool = false) (pattern : string) (ty : typ) : Transfo.t =
  applyto_fields_type ~reparse pattern (fun _ -> ty)

(* [simpl_proj tg] expects the target [tg] pointing to any node whose descendants can contain struct
 initialization list projections 
*)
let simpl_proj : Transfo.t =
  apply_on_targets (Struct_core.simpl_proj)

(* [struct_modif new_fields f_get f_set use_annot_of tg]: expects the target [tg] to point at a typedef struct, 
    then it will replace its current fields with [new_fields]. After modifying the fields it will search for 
    accesses of the targeted struct and modify them, if they are surrounded by a set operation it will apply 
    [f_set] on that access otherwise [f_get] is going to be applied
  *)

let struct_modif ?(use_annot_of : bool = false) ?(new_fields : (label * typ) list = []) ~f_get:(f_get : trm -> trm) 
  ~f_set:(f_set : trm -> trm) : Transfo.t = 
  apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
  (fun t (p, i) -> Struct_core.struct_modif new_fields f_get f_set use_annot_of i t p)

