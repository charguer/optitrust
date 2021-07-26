open Ast

let hoist (x_step : var) (tg : Target.target) : unit =
  Internal.nobrace_enter ();
  Target.apply_on_transformed_targets(Internal.get_trm_in_surrounding_loop)
    (fun (p, i) t -> 
      let (tg_trm,_) = Path.resolve_path (p @ [Dir_body;Dir_seq_nth i]) t in
      (* Check if detaching is needed *)
      let detach_first = 
      match tg_trm.desc with 
      | Trm_let (_, (_, _), init) ->
        begin match init.desc with 
        | Trm_val(Val_lit (Lit_uninitialized)) -> false
        | Trm_val(Val_prim (Prim_new _))-> false
        | _ -> true
        end
      | _ -> fail tg_trm.loc "hoist: expected a variable declaration" 
      in
      match detach_first with 
      | true -> 
        let t = Generic_core.var_init_detach i t (p @ [Dir_body]) in
        let t = Loop_core.hoist x_step i t p in t
      | false -> let t = Loop_core.hoist x_step i t p in t) tg;
  Internal.nobrace_remove_and_exit ()