open Ast

(* [set_explicit tg] expects [tg] to point to a set instruction where one struct 
    instance has been assigned anothe struct instance. Or a variable declaration of type struct
    with initialization. If this is the case then first a detachement is performed.
*)
let set_explicit (tg : Target.target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> 
      let (tg_trm, _) = Path.resolve_path (p @ [Dir_seq_nth i]) t in
      match tg_trm.desc with 
      | Trm_let _ ->
        let t = Variable_core.init_detach i t p in
                Struct_core.set_explicit (i + 1) t p
      | _ -> Struct_core.set_explicit i t p
    ) tg


(*  [set_implicit tg] expects [tg] to point to a struct set operation, with the assumption
      that imediately after this instruction come all the other set instruction for the same 
      struct set operation. The trasnformation is going to find the type of the instruction
      and then consider (n - 1) instructions after the targeted instruction. Where n is the number
      of struct fields of the type of the instruction.
*)
let set_implicit (tg : Target.target) : unit =
  let typid_to_typedef_map = Clang_to_ast.(!ctx_typedef) in
   
  let t = Trace.get_ast () in
  let tg_path = Target.resolve_target_exactly_one tg t in
  let (tg_trm, _) = Path.resolve_path tg_path t in
  match tg_trm.desc with 
  | Trm_apps (_, [lt;rt]) ->
    let tid_r = Internal.get_typid_from_trm rt  in 
    let tid_l = Internal.get_typid_from_trm lt  in
    let tid = match tid_r, tid_l with 
    | -1, _ -> tid_l
    | _, -1 -> tid_r
    | _, _ -> if tid_r = tid_l then tid_r else fail t.loc "set_explicit_aux: different types in an assignment"
    in
    let struct_def = Typ_map.find tid typid_to_typedef_map in
    let field_list = Internal.get_field_list struct_def in
    let nb = List.length field_list in
    Sequence_basic.intro nb tg;
    Struct_basic.set_implicit [Target.cSeq ~args_pred:(Target.target_list_one_st (Internal.get_constr_from_target tg)) ()]
  | _ -> fail tg_trm.loc "set_implicit: expected a set operation"
