open Prelude
include Record_basic

(** [split_fields]: an extension to [Record_basic.split_fields].
  It takes as argument ~(typ : typ) instead of ~(typ : typvar).
   *)
let%transfo split_fields ?(typ : typ option) ?(typs : typ list = []) (tg : target) : unit =
  let typs = match (typ, typs) with
  | None, [] -> failwith "expected at least one type to split on"
  | None, _ -> typs
  | Some typ, [] -> [typ]
  | Some _, _ -> failwith "both ~typ and ~typs were given"
  in
  List.iter (fun typ ->
    Pattern.pattern_match typ [
      Pattern.(trm_var !__) (fun typ () ->
        split_fields ~typ tg
      );
      Pattern.__ (fun () ->
        trm_fail typ "expected type variable"
      )
    ]
  ) typs

(** [set_explicit tg]: an extension to [Record_basic.set_explicit](see Record_basic.ml), contrary to the basic
    on this transformation supports automatic variable declaration detachment.
    vect v = {0,0}; becomes vect v; v.x = 0; v.y = 0; *)
let set_explicit (tg : Target.target) : unit =
  Target.iter (fun p ->
    let surrounding_seq,_ = Internal.isolate_last_dir_in_seq p in
    let tg_trm = Target.resolve_path p in
      begin match tg_trm.desc with
      | Trm_let ((x,tx), _) ->
        if is_reference tx then Tools.debug "WARNING: set_explicit on a reference can only be correct if the reference is used for read-only purpose";
        Variable_basic.init_detach (Target.target_of_path p);
        Record_basic.set_explicit ((Target.target_of_path surrounding_seq) @ [Target.cStrict;Target.cWriteVar x.name])
      | _ -> Record_basic.set_explicit (Target.target_of_path p)
      end

  ) tg

(** [set_implicit tg]: an extension to [Record_basic.set_implicit](see Record_basic.ml), contrary to the basic one
     this one expects that the target [tg] matches all the write operations that can be converted to a single
     write operation. *)
let set_implicit (tg : Target.target) : unit =
  Target.iter (fun p ->
    let tg_trm = Target.resolve_path p in
    match tg_trm.desc with
    | Trm_apps (_, [lt;rt], _, _) ->
      let tid_r = Internal.get_typvar_from_trm ~first_match:false rt in
      let tid_l = Internal.get_typvar_from_trm ~first_match:false lt in
      let tvar = match tid_r, tid_l with
      | None, Some tvar | Some tvar, None -> tvar
      | Some tvar_r, Some tvar_l ->
        if not (var_eq tvar_r tvar_l) then trm_fail tg_trm "set_explicit_aux: different types in an assignment";
        tvar_r
      | None, None ->
        trm_fail tg_trm "set_explicit_aux: explicit assignment is supported only for struct types"
      in
      let struct_def =
        match Internal.typvar_to_typedef tvar with
        | Some td -> td
        | _ -> trm_fail tg_trm (sprintf "set_explicit_aux: could not get the declaration of typedef %s" (var_to_string tvar))
      in
      let field_list = Internal.get_field_list struct_def in
      let nb = List.length field_list in
      Sequence_basic.intro ~mark:"__SEQUENCE_MARK" nb tg;
      Record_basic.set_implicit [Target.cMark "__SEQUENCE_MARK"];
    | _ -> trm_fail tg_trm "Record.set_implicit: expected a set operation"

) tg


(** [rename_field field ~into tg]: this is a specialization of [Record_basic.rename_fields]
      when one wants to rename only one field of a Record. [field] is the current field name
      [into] is the new name that is going to replace all the occurrences of field in the context of
      the targeted typedef Record. *)
let rename_field (field : field) ~(into : string): target -> unit =
  rename_fields (only_for field (fun _ -> into))


(** [align_field align pattern tg]: expects the target [tg] to be pointing at a typedef struct definition,
   then it will align all the fields that match [pattern] with [align] size. *)
let align_field (align : trm) (pattern : string) : target -> unit =
  Record_basic.applyto_fields_type pattern (fun ty -> typ_align align ty)
