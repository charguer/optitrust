open Prelude
open Target
include Record_core
include Record_core.Rename

(** [set_explicit tg]: expects the target [tg] to point at a set instruction where one struct
    instance has been assigned another struct instance. *)
let%transfo set_explicit (tg : target) : unit =
  Nobrace_transfo.remove_after ( fun _ ->
    apply_at_target_paths (Record_core.set_explicit_on) tg)

(** [set_implicit tg]: expects the target [tg] to point at a sequence containing
      a list of struct set assignments. And transforms it into a single struct assignment.
      So it is the inverse of set_explicit. *)
let%transfo set_implicit (tg : target) : unit =
  apply_at_target_paths (Record_core.set_implicit_on) tg

(** [reorder_fields order tg]: expects the target to be pointing at typedef struct or class.
      then it changes the order of the fields based on [order].
      [order] - can be one of the following
        Move_before (x,fl) -> all the fields that belong to [fl] are moved before the field [x].
        Move_after (x,fl) -> all the fields that belong to [fl] are moved after the field [x].
        Reorder_all [fl] -> all the fields are reorder an will appear like [fl].

    @correctness: Correct if pointer arithmetic to field is replaced everywhere,
      might be impossible to prove in case of casts between types.*)
let%transfo reorder_fields (order : fields_order) (tg : target) : unit =
  apply_at_target_paths_in_seq (Record_core.reorder_fields_at order) tg

(** [reveal_field ~reparse field_to_reveal_field tg]: expects the target [tg] to point at a typedef struct,
    then it will find [field_to_reveal_field] and it's underlying type and it will
    replace [field_to_reveal_field] with a list of fields rename comming from its underlying type. *)
let%transfo reveal_field ?(reparse:bool=false) (field_to_reveal_field : field) (tg : target) : unit =
  reparse_after ~reparse
    (apply_at_target_paths_in_seq (Record_core.reveal_field_at field_to_reveal_field))
    tg

(** [reveal_fields fields_to_reveal_field tg]: an extension to the reveal_field transformation, this one
     is applied on multiple struct fields. *)
let%transfo reveal_fields ?(reparse : bool = false) (fields_to_reveal_field : fields) (tg : target) : unit =
  List.iter (fun f -> reveal_field f tg) fields_to_reveal_field


(** [to_variables tg]: expects the target [tg] to point at a variable declaration of type typedef Record.
    Then it will transform this declaration into a list of variable declarations where the type
    of these variables is inherited from the type of the struct definition. All the struct_accesses
    are going to be changed to variable occurrences. *)
let%transfo to_variables (tg : target) : unit =
  apply_at_target_paths_in_seq (Record_core.to_variables_at) tg

(** [rename_fields rename tg] expects the target [tg] to point at a struct declaration,
    then it will rename all the fields that are matched when applying the type [rename]
    which can be a function to rename all the struct fields or only those that
    are matched by the [pattern] given as argument when the function [only_for] is used (see struc_core.ml). *)
let%transfo rename_fields (rename : rename) (tg : target) : unit =
  apply_at_target_paths_in_seq (fun i t -> Record_core.rename_fields_at i rename t) tg

(** [applyto_fields_type ~reparse pattern typ_update tg]: expects the target [tg] to point at a
    struct definition, then it will update all the struct field types whose identifier matches [pattern]. *)
let%transfo applyto_fields_type ?(reparse : bool = false) (pattern : string) (typ_update: typ -> typ) (tg : target) : unit =
  reparse_after ~reparse (apply_at_target_paths (Record_core.update_fields_type_on pattern typ_update)) tg

(** [update_fields_type pattern ty tg]: expects the target [tg] to point at a struct declaration,
    then it will change the current type to [ty] for all the fields that are matched by [pattern]. *)
let update_fields_type ?(reparse : bool = false) (pattern : string) (ty : typ) (tg : target) : unit =
  applyto_fields_type ~reparse pattern (fun _ -> ty) tg

(** [simpl_proj tg]: expects the target [tg] to point at any node whose descendants can contain struct
    initialization list projections. *)
let%transfo simpl_proj (tg : target) : unit =
  Resources.justif_correct "arguments are pure/reproducible";
  apply_at_target_paths (Record_core.simpl_proj_on) tg

(** [struct_modif new_fields f_get f_set use_annot_of tg]: expects the target [tg] to point at a typedef struct,
    then it will replace its current fields with [new_fields]. After modifying the fields it will search for
    accesses of the targeted struct and modify them, if they are surrounded by a set operation it will apply
    [f_set] on that access otherwise [f_get] is going to be applied. *)
let%transfo struct_modif (arg : Struct_modif.arg) (tg : target) : unit =
  apply_at_target_paths_in_seq (Record_core.struct_modif_at arg) tg

(* FUTURE
let struct_modif_simple ?(use_annot_of : bool = false) ?(new_fields : (label * typ) list = [])       ~ ?f_get:(f_get : (trm -> trm) option) ?f_set:(f_set : (trm -> trm) option) : target -> unit =
  struct_modif {
    f_get = (match f_get with Some f ->
         (fun aux t -> let t' = f aux t in
            if use_annot_of then { t' with annot = t.annot })
      | None -> (fun _ _ -> assert false));
    f_set: modif;
    f_struct_get: modif:
    f_access: modif;
 *)


(** [change_field_access_kind acc_kind f tg]: expects the target [tg] to point a typedef, then it will find
    field [f] at change its access kind to [acc_kind]. *)
let%transfo change_field_access_kind ?(field : field = "") (acc_kind : record_field_annot) (tg : target) : unit =
  apply_at_target_paths (Record_core.change_field_access_kind_on acc_kind field) tg

(** [make_all_members_public tg]: expects the target [tg] to point at a typedef struct or class.
    then it will transform all its members to public. *)
let make_all_memebers_public : target -> unit =
  change_field_access_kind Access_public

(** [method_to_const method_name]: expects the target [ŧg] to be pointing at a typedef record definition.
    Then it will check if the method of that record definition is already a const method or not.
    If it's a const method then this transformation does nothing, otherwise it will transform that method to a const one.
    Note: If [method_name] is not specified by the user all the methods will be converted to const methods.*)
let%transfo method_to_const ?(method_name : var = dummy_var) (tg : target) : unit =
  apply_at_target_paths (Record_core.method_to_const_on method_name) tg
