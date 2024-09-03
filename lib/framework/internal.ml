open Prelude
open Target

(** [typvar_to_typedef tid]: gets the declaration of a typedef with name [var]. *)
let typvar_to_typedef (var : typvar) : typedef option =
  let rec aux (t : trm) : typedef option =
    match t.desc with
    | Trm_seq tl -> Mlist.find_map aux tl
    | Trm_namespace (_, t, _) -> aux t
    | Trm_typedef ({ typedef_name } as td) when var_eq typedef_name var -> Some td
    | _ -> None
  in
  let t_root = Target.get_ast () in
  match t_root.desc with
  | Trm_seq _ -> aux t_root
  | _ -> trm_fail t_root "Internal.typvar_to_typedef: expected the ast of the main file to be a sequence"

(** [record_typ_to_typvar ty]: gets the name of the record type [ty]. This function goes through aliases. *)
(* Is this function useful ? *)
let rec record_typ_to_typvar (ty : typ) : typvar option =
  Pattern.pattern_match ty [
    Pattern.(typ_const !__) (fun ty () -> record_typ_to_typvar ty);
    Pattern.(typ_ptr !__) (fun ty () -> record_typ_to_typvar ty);
    Pattern.(typ_var !__) (fun var () ->
      match typvar_to_typedef var with
      | Some { typedef_body = Typedef_alias ty } -> record_typ_to_typvar ty
      | Some { typedef_body = Typedef_record _ } -> Some var
      | _ -> None
    );
    Pattern.__ (fun () -> None)
  ]

let trm_map_at ?(change_at : target list option) (f_map: trm -> trm) (t: trm) : trm =
  match change_at with
  | None -> f_map t
  | Some change_at ->
    let res = List.fold_left
      (fun t' tr ->
        let tr = if not (List.mem nbAny tr)
          then [nbAny] @ tr
          else tr in
        let epl = Constr.resolve_target tr t' in
        match epl with
        | [] ->
          Flags.verbose_warn t'.loc "Internal.change_trm: no matching subterm for target %s"
            (target_to_string tr);
          t'
        | _ -> List.fold_left (Path.apply_on_path f_map) t' epl
      ) t change_at
    in
    res

(** [change_trm ~change_at t_before t_after t]: replace all the occurrences of [t_before] with [t_after]
   If [change_at] is not equal to [[]] then this function is applied only to descendants of the trm corresponding to
   the targets [change_at] *)
let change_trm ?(change_at : target list option) (t_before : trm)
  (t_after : trm) (t : trm) : trm =
  let rec apply_change (t' : trm) : trm=
    if are_same_trm t' t_before then
      trm_copy t_after
    else trm_map apply_change t'
  in
  trm_map_at ?change_at apply_change t

(** [change_typ ~change_at ty_before ty_after t]: similar to [change_trm] but for types *)
let change_typ ?(change_at : target list option) (ty_before : typ)
  (ty_after : typ) (t : trm) : trm =
  change_trm ?change_at ty_before ty_after t

(** [isolate_last_dir_in_seq dl]: for a trm with path [dl] return the path to the surrouding sequence of the
    instruction that contains that trm, and the index of that instruction on that sequence *)
let isolate_last_dir_in_seq (dl : path) : path * int =
    match List.rev dl with
    | Dir_seq_nth i :: dl' -> (List.rev dl',i)
    | Dir_record_field _ :: Dir_seq_nth i :: dl'  -> (List.rev dl', i)
      (* Tools.debug "Path: %s" (Path.path_to_string dl); *)
    | _ ->
      path_fail dl "Internal.isolate_last_dir_in_seq: the transformation expects a target on an element that belongs to a sequence"
  (* LATER: raise an exception that each transformation could catch OR take as argument a custom error message *)

(** [get_instruction_in_surrounding_sequence dl]: for a trm with path [dl] return the path to the surrouding sequence
     of the instruction that contains that trm, the path from that instruction to the trm itself and the index of that
     instruction on that sequence. *)
let get_instruction_in_surrounding_sequence (dl : path) : path * path * int =
  let rec aux (acc : path) (dl : path) =
    match dl with
    | [] -> path_fail dl "Internal.get_instruction_in_surrounding_sequence: empty path"
    | Dir_seq_nth i :: dl'-> (List.rev dl', acc, i)
    | dir :: dl' -> aux (dir :: acc) dl'
  in aux [] (List.rev dl)

(** [get_ascendant_path checker dl t]: for a trm with path [dl] return the path to the first ascendant that satisfies
    that satisfies the predicate [checker]. *)
let get_ascendant_path (checker : trm -> bool) (dl : path) (t : trm) : path =
  let rec aux (dl1 : path) : path =
    match dl1 with
    | [] -> []
    | hd_p :: tl_p ->
      let res = Path.resolve_path (List.rev dl1) t in
        if checker res then (List.rev dl1) else aux tl_p
    in
  aux (List.rev dl)

(** [get_surrouding_access dl t]: specialization of get_ascendant_path for accesses*)
let get_ascendant_access_path (dl : path) (t : trm) : path =
  get_ascendant_path is_access dl t

(** [get_ascendant_read_path dl t] specialization of get_ascendant_path for read operations*)
let get_ascendant_read_path (dl : path) (t : trm) : path =
  get_ascendant_path is_get_operation dl t

(** [get_ascendant_write_path dl t] specialization of get_ascendant_path for write operations*)
let get_ascendant_write_path (dl : path) (t : trm) : path =
  get_ascendant_path is_set_operation dl t

(** [get_ascendant_topfun_path dl]: returns the path to the toplevel function that contains
     the trm where the path [dl] points to. *)
let get_ascendant_topfun_path (dl : path) : path option =
  match dl with
  | Dir_seq_nth i :: Dir_body :: _ -> Some [Dir_seq_nth i]
  | _ -> None

(** [is_decl_body dl]: check if [dl] points to a declaration body *)
let is_decl_body (dl : path) : bool =
  match List.rev dl with
  | Dir_body :: _ -> true
  | _ -> false

(** [get_field_list td]: in the case of typedef struct give back the list of struct fields *)
let get_field_list (td : typedef) : (field * typ) list =
  match td.typedef_body with
  | Typedef_record rfl ->
    List.map (fun (rf, _) ->
      match rf with
      | Record_field_member (lb, ty) -> (lb, ty)
      | _ -> failwith "Internal.get_field_list: expected a struct without methods"
    ) rfl
  | _ -> failwith "Internal.get_field_list: expected a Typedef_prod"


(** [get_typvar_from_typ t]: check if typ is a constructed type or a composed type
    In case it is constructed type then return its id.
    In case it is a composed type go in depth and check if it contains a constructed type and return its id.
    Otherwise return None meaning that the type [t] is not a constructed type.

    DEPRECATED: it is here only for [get_typvar_from_trm] implementation, you should probably use [Typ.typ_constr_inv] instead
*)
let rec get_typvar_from_typ (ty : typ) : typvar option =
  Pattern.pattern_match ty [
    Pattern.(typ_var !__) (fun var () -> Some var);
    Pattern.(typ_const !__) (fun ty () -> get_typvar_from_typ ty);
    Pattern.(typ_ptr !__) (fun ty () -> get_typvar_from_typ ty);
    Pattern.(typ_array !__ __) (fun ty () -> get_typvar_from_typ ty);
    Pattern.(typ_fun __ !__) (fun ty () -> get_typvar_from_typ ty); (* FIXME: This case is weird *)
    Pattern.(typ_apps !__ __) (fun var () -> Some var);
    Pattern.__ (fun () -> None)
  ]

(** [get_typvar_from_trm ~first_martch t]: for trm [t] check if its type is a constructed type.
   If that's the case then return its constructor name, otherwise return [None], meaning that trm [t] has a different typ.

  FIXME: This function is very badly designed it is unclear if one wants to get the type inside or outside a field access.
  Now, almost all types are constructed types because primitive types are not really special, and should not be treated as such.
  Do not use in new code !
*)
let rec get_typvar_from_trm ?(first_match : bool = true) (t : trm) : typvar option =
  match t.desc with
  | Trm_apps (_,[base], _) ->
    begin match t.typ with
    | Some typ ->
      begin match get_typvar_from_typ typ with
      | Some var -> Some var
      | None -> if first_match then None else get_typvar_from_trm base
      end
    | None -> get_typvar_from_trm base
    end
  | Trm_let ((_,tx),_) -> get_typvar_from_typ tx
  | Trm_record _ | Trm_var _ ->
      begin match t.typ with
      | Some ty -> get_typvar_from_typ ty
      | _ -> None
      end
  | _ -> None


(** [toplevel_decl ~require_body x]: finds the toplevel declaration of variable x, x may be a function or variable.
    If [require_body] is set to true, then only definitions are considered.*)
let toplevel_decl ?(require_body:bool=false) (x : var) : trm option =
  let full_ast = Target.get_ast () in
  let aux (t1 : trm) : trm option =
    match t1.desc with
    | Trm_let ((y, _), _) when var_eq y x -> Some t1
    | Trm_let_fun (y, _, _, body, _) when var_eq y x ->
      if require_body then begin
        match body.desc with
        | Trm_seq _ -> Some t1 (* LATER: we might want to test insted if body.desc <> trm_uninitialized or something like that *)
        | _ -> None
      end else begin
        Some t1
      end
    | _ -> None
   in
  match full_ast.desc with
  | Trm_seq tl ->
    Mlist.fold_left(
      fun acc t1 ->
      match acc with
      | Some _ -> acc
      | _ -> aux t1
  ) None tl
  | _ -> trm_fail full_ast "Internal.top_level_decl: the full ast starts with the main sequence which contains all the toplevel declarations"


(** [local_decl x t]: check if [t] is a declaration with name [x], if that's the case the return that declaration *)
let rec local_decl (x : var) (t : trm) : trm option =
  match t.desc with
  | Trm_let ((y, _), _) when var_eq y x -> Some t
  | Trm_let_fun (y, _, _, body, _) ->
    if var_eq y x then Some t else local_decl x body
  | Trm_seq tl ->
    Mlist.fold_left(
      fun acc t1 ->
      match acc with
      | Some _ -> acc
      | _ ->
        let t2 = local_decl x t1 in
        begin match t2 with
        | Some _->  t2
        | _ -> None
        end
    ) None tl
  | _ -> None

(* LATER: Use trm_fors_inv instead *)
(** [get_loop_nest_indices t]: if node [t] represents a loop nest then go through all of them an return an
    ordered list of their indices where the order is the depth order *)
let rec get_loop_nest_indices (t : trm) : 'a list =
  match t.desc with
  | Trm_for (range, body, _) ->
    begin match body.desc with
    | Trm_seq tl when Mlist.length tl = 1  ->
      let f_loop = Mlist.nth tl 0 in
      range.index :: get_loop_nest_indices f_loop
    | _ -> range.index :: []
    end
  | Trm_for_c (_, _, _, body, _) ->
    let index = for_loop_index t in
    begin match body.desc with
    | Trm_seq tl when Mlist.length tl = 1 ->
      let f_loop = Mlist.nth tl 0 in
      index :: get_loop_nest_indices f_loop
    | _ -> index :: []
    end
  | _ -> []

(** [extract_loop t]: for a loop [t] return a pair of function and trm. Where the function takes trm b
    and gives a loop with the same components as loop [t] but with body b. And the trm is the body of the loop [t]. *)
let extract_loop (t : trm) : ((trm -> trm) * trm) option =
  match t.desc with
  | Trm_for_c (init, cond, step, body, _) ->
    Some ((fun b -> trm_for_c init cond step b), body)
  | Trm_for (l_range, body, _) ->
    Some ((fun b -> trm_for l_range b), body)
  | _ ->
    trm_fail t "Internal.extract_loop: expected a loop"

(** [get_field_index field fields]: for a struct field with name [field] and [fields] being the list of fields of the
    same struct, return back the index of field [field] in the list of fields [fields]. *)
let get_field_index (t : trm) (field : field) (fields : record_fields) : int =
  let rec aux field fields c = match fields with
    | [] -> trm_fail t "Internal.get_field_index: empty list"
    | (rf, _) :: tl ->
      begin match rf with
      | Record_field_member (f, _) ->
        if f = field then c else aux field tl (c + 1)
      | _ -> aux field tl (c+1)
      end
    in
  aux field fields 0


(** [apply_on_record_fields app_fun rfs]: applies [app_fun] on all the elements of [rfs]. *)
let apply_on_record_fields (app_fun : record_field -> record_field ) (rfs : record_fields) : record_fields =
  List.map (fun (rf, rf_annot) -> (app_fun rf, rf_annot)) rfs


(** [rename_record_fields]: renames all the fields [rfs] by applying function [rename_fun]. *)
(* FIXME: #var-id , sets id to inferred_var_id, requiring id inference afterwards. *)
let rename_record_fields (rename_fun : string -> string) (rfs : record_fields) : record_fields =
  let app_fun (rf : record_field) : record_field =
    match rf with
    | Record_field_member (f, ty) -> Record_field_member (rename_fun f, ty)
    | Record_field_method t ->
      begin match t.desc with
      | Trm_let_fun (fn, ret_ty, args, body, contract) ->
        let new_fn = { namespaces = fn.namespaces; name = (rename_fun fn.name); id = unset_var_id } in
        let new_t = trm_alter  ~desc:(Trm_let_fun (new_fn, ret_ty, args, body, contract)) t in
        Record_field_method new_t
      | _ -> trm_fail t "Internal.rename_record_fields: record member not supported."
      end
  in
  apply_on_record_fields app_fun rfs

(** [update_record_fields_type typ_update rfs]: updates the type of [rfs] based on [typ_update] function. *)
let update_record_fields_type ?(pattern : string = "")(typ_update : typ -> typ ) (rfs : record_fields) : record_fields =
  let app_fun (rf : record_field) : record_field =
    match rf with
    | Record_field_member (f, ty) ->
      let ty = if Tools.pattern_matches pattern f then typ_update ty else ty in
      Record_field_member (f, ty)
    | Record_field_method t -> trm_fail t "Internal.update_record_fields_type: can't update the type of a method."
      in
    apply_on_record_fields app_fun rfs

(** [change_loop_body loop body]: change the current body of loop [loop] with [body] *)
let change_loop_body (loop : trm) (body : trm) : trm =
  match loop.desc with
  | Trm_for (l_range, _, contract) ->
    trm_for ~contract l_range body
  | Trm_for_c (init, cond, step, _, invariant) ->
    trm_for_c ?invariant init cond step body
  | _-> trm_fail loop "Internal.change_loop_body: expected for loop"

(** [is_trm_loop t] check if [t] is a loop or not *)
let is_trm_loop (t : trm) : bool =
  match t.desc with
  | Trm_for _ | Trm_for_c _ -> true
  | _ -> false

(** [get_constr_from_target tg]: get the constraint from a list of constraints(targets) *)
let get_constr_from_target (tg : target) : constr =
  match tg with
  | [cnst] -> cnst
  | _ -> cTarget tg

(** [replace_return_with_assign exit_label r t]: removes all the return statements from the body of a function declaration,
      [exit_label] - generated only if [t] is there is a sequence that contains not terminal instructions,
      [r] - the name of the variable replacing the return statement, can be [dummy_var] to ... ?
      [t] - ast of the body of the function. *)
let replace_return_with_assign ?(check_terminal : bool = true) ?(exit_label : label = no_label) (r : var) (t : trm) : (trm * int) =
  let nb_gotos = ref 0 in
  let rec aux (is_terminal : bool) (t : trm) : trm =
    match t.desc with
    | Trm_abort ab ->
      begin match ab with
      | Ret t1 ->
        begin match t1 with
        | Some t2 ->
          let t1' = (aux false t2) in
          let t_assign = if r = dummy_var then t2 else trm_set (trm_var r) t1' in
          if is_terminal
            then t_assign
            else begin
                 incr nb_gotos;
                 if exit_label = no_label then t_assign else trm_seq_nobrace_nomarks [t_assign; trm_goto exit_label]
                 end
        | _ ->
            incr nb_gotos;
            if exit_label = no_label then trm_unit () else trm_goto exit_label
        end
      | _ ->
          incr nb_gotos;
          if exit_label = no_label then trm_unit () else trm_goto exit_label
      end
    | Trm_let_fun _ -> t (* do not recurse through local function definitions *)
    | _-> trm_map_with_terminal is_terminal aux t
  in
  let t = aux check_terminal t in
  (t, !nb_gotos)


(** [get_field_name rf]: returns the name of the field [rf]. *)
let get_field_name (rf : record_field) : string option =
  match rf with
  | Record_field_member (n, _) -> Some n
  | Record_field_method t1 ->
    (* CHECK: #var-id *)
    begin match t1.desc with
    | Trm_let ((n, _), _) -> Some n.name
    | Trm_let_fun (qn, _, _, _, _) -> Some qn.name
    | _ -> None
    end
