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
        let t = Generic_core.var_init_detach i t p in
                Struct_core.set_explicit (i + 1) t p
      | _ -> Struct_core.set_explicit i t p
    ) tg
