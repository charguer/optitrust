open Ast
include Struct_basic

(* [set_explicit tg] expects [tg] to point to a set instruction where one struct
    instance has been assigned to another struct instance. Or a variable declaration of type struct
    with initialization. If this is the case then first a detachement is performed.
*)
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

(*  [set_implicit tg] expects [tg] to point to a struct set operation, with the assumption
      that this instruction is folowed by all the other set instruction for the same
      struct set operation. The trasnformation is going to find the type of the instruction
      and then consider (n - 1) instructions after the targeted instruction. Where n is the number
      of struct fields of the type of the instruction.
*)
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
    | _ -> fail tg_trm.loc "set_implicit: expected a set operation"

) tg