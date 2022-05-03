open Ast
include Struct_basic

(* [set_explicit tg]: an extension to [Struct_basic.set_explicit](see struct_basic.ml), contrary to the basic 
    on this transformation supports automatic variable declaration detachment.
    vect v = {0,0}; becomes vect v; v.x = 0; v.y = 0; *)
let set_explicit (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let surrounding_seq,_ = Internal.isolate_last_dir_in_seq p in
    let tg_trm = Path.resolve_path p t in
      begin match tg_trm.desc with
      | Trm_let (_, (x,tx), _) ->
        if is_reference tx then Printf.printf "WARNING: set_explicit on a reference can only be correct if the reference is used for read-only purpose\n";
        Variable_basic.init_detach (Target.target_of_path p);
        Struct_basic.set_explicit ((Target.target_of_path surrounding_seq) @ [Target.cStrict;Target.cWriteVar x])
      | _ -> Struct_basic.set_explicit (Target.target_of_path p)
      end

  ) tg

(* [set_implicit tg]: an extension to [Struct_basic.set_implicit](see struct_basic.ml), contrary to the basic one
     this one expects that the target [tg] matches all the write operations that can be converted to a single 
     write operation *)
let set_implicit (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_apps (_, [lt;rt]) ->
      let tid_r = Internal.get_typid_from_trm ~first_match:false rt  in
      let tid_l = Internal.get_typid_from_trm ~first_match:false  lt  in
      let tid = match tid_r, tid_l with
      | -1, _ -> tid_l
      | _, -1 -> tid_r
      | _, _ -> if tid_r = tid_l then tid_r else fail t.loc "set_explicit_aux: different types in an assignment"
      in
      let struct_def =
        if tid <> -1 then match Context.typid_to_typedef tid with
          | Some td -> td
          | _ -> fail t.loc "set_explicit_aux: could not get the declaration of typedef"
        else
          fail t.loc "set_explicit_aux: explicit assignment is supported only for struct types"
      in
      let field_list = Internal.get_field_list struct_def in
      let nb = List.length field_list in
      Sequence_basic.intro ~mark:"__SEQUENCE_MARK" nb tg;
      Struct_basic.set_implicit [Target.cMark "__SEQUENCE_MARK"];
    | _ -> fail tg_trm.loc "Struct.set_implicit: expected a set operation"

) tg


(* [rename_field field ~into tg]: this is a specialization of [Struct_basic.rename_fields] 
      when one wants to rename only one field of a struct. [field] is the current field name
      [into] is the new name that is going to replace all the occurrences of field in the context of
      the targeted typedef struct. *)
let rename_field (field : field) ~into:(into : var): Target.Transfo.t =
  rename_fields (only_for field (fun _ -> into))


(* [align_field align pattern tg]: expects the target [tg] to be pointing at a typedef struct definition,
   then it will align all the fields that match [pattern] with [align] size. *)
let align_field (align : trm) (pattern : string) : Target.Transfo.t = 
  Struct_basic.applyto_fields_type pattern (fun ty -> typ_align align ty) 
